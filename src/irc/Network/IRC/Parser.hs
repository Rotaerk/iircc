{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module: Network.IRC.Parser
Description: An attoparsec parser for IRC messages.
-}
module Network.IRC.Parser (
  TaggedParser,
  TPrefix,
  Command,
  Param,
  Host,
  User,
  NickName,
  ServerName,
  pMessage,
  tpPrefix
) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString as ABS
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Composition
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid
import Data.Tagged
import Data.Word

{-
We're basing this parser on the syntax specified in https://tools.ietf.org/html/rfc2812, but there will be times that we have to adjust it to account for real-world deviations from the specification.

From the spec:

  The Augmented BNF representation for this is:

  message    =  [ ":" prefix SPACE ] command [ params ] crlf
  prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
  command    =  1*letter / 3digit
  params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
             =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]
  nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
                  ; any octet except NUL, CR, LF, " " and ":"
  middle     =  nospcrlfcl *( ":" / nospcrlfcl )
  trailing   =  *( ":" / " " / nospcrlfcl )
  SPACE      =  %x20        ; space character
  crlf       =  %x0D %x0A   ; "carriage return" "linefeed"
  target     =  nickname / server
  msgtarget  =  msgto *( "," msgto )
  msgto      =  channel / ( user [ "%" host ] "@" servername )
  msgto      =/ ( user "%" host ) / targetmask
  msgto      =/ nickname / ( nickname "!" user "@" host )
  channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring
                [ ":" chanstring ]
  servername =  hostname
  host       =  hostname / hostaddr
  hostname   =  shortname *( "." shortname )
  shortname  =  ( letter / digit ) *( letter / digit / "-" )
                *( letter / digit )
                  ; as specified in RFC 1123 [HNAME]
  hostaddr   =  ip4addr / ip6addr
  ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
  ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
  ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
  nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
  targetmask =  ( "\$" / "#" ) mask
                  ; see details on allowed masks in section 3.3.1
  chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B
  chanstring =/ %x2D-39 / %x3B-FF
                  ; any octet except NUL, BELL, CR, LF, " ", "," and ":"
  channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )
  user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
                  ; any octet except NUL, CR, LF, " " and "@"
  key        =  1*23( %x01-05 / %x07-08 / %x0C / %x0E-1F / %x21-7F )
                  ; any 7-bit US\_ASCII character,
                  ; except NUL, CR, LF, FF, h/v TABs, and " "
  letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
  digit      =  %x30-39                 ; 0-9
  hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"
  special    =  %x5B-60 / %x7B-7D
                   ; "[", "]", "\", "`", "\_", "\^", "{", "|", "}"

Below, we will have parser-building functions that abstract the representations of the parse results.  This means that the parsers' result types will be generic, and other generic parsers will be provided as inputs along with a function from the user for constructing the representation from its parts.  This allows the caller to use their own data types and to extract whatever information they want, discarding the rest.  However, this removes some type safety, allowing the wrong parsers to be used as arguments to the larger parser.  Thus, we tag the parser in order to re-introduce this type information where needed.
-}

type TaggedParser tag a = Tagged tag (Parser a)

{-

What follows is a parser for each term in the order they appear in the spec, but first, some parsers for character literals, and some other utility functions:

-}

[bSpace, bUpperA, bUpperF, bUpperZ, bLowerA, bLowerZ, bZero, bNine, bHyphen] = BS.unpack " AFZaz09-"
[pExclam, pColon, pAt, pPeriod, pCR, pLF] = word8 <$> BS.unpack "!:@.\CR\LF"

inRange :: Word8 -> Word8 -> Word8 -> Bool
inRange lb ub b = lb <= b && b <= ub

{-

Message

message    =  [ ":" prefix SPACE ] command [ params ] crlf

* We treat the SPACE as part of the prefix, and don't specify it here, because the prefix parser needs to know where it ends.
* We don't mark the params rule as optional because it is inherently so.

-}

pMessage
  :: TaggedParser TPrefix prefix
  -> (Maybe prefix -> Command -> [Param] -> message)
  -> Parser message
pMessage tpPrefix buildMessage =
  (<?> "Message") $
  buildMessage <$>
  optional (pColon *> unTagged tpPrefix) <*>
  pCommand <*>
  pParams <* pMessageSeparator

{-
Prefix

prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )

* We add the SPACE (term-separator) rule to the end of the prefix parser rather than outside of it in the message parser because it needs to be parsed per alternative.  Without doing this, the parser of the first option will succeed on a partial name, and then the message parser will fail because there isn't a SPACE immediately afterward.
* We reverse the order of the alternatives because nicknames are valid servernames, and we are choosing to assume, in the case where it could be either, that it is a nickname.  We're uncertain whether this will be satisfactory.

-}

data TPrefix

tpPrefix
  :: (NickName -> Maybe (Maybe User, Host) -> prefix)
  -> (ServerName -> prefix)
  -> TaggedParser TPrefix prefix
tpPrefix fromNickName fromServerName =
  Tagged . (<?> "Prefix") $
  choice . fmap (<* pSpaces) $
  [pPrefixNick, pPrefixServerName]
  where
    pPrefixServerName = fromServerName <$> pServerName
    pPrefixNick =
      fromNickName <$>
      pNickName <*>
      optional (
        (,) <$>
        optional (pExclam *> pUser) <*>
        (pAt *> pHost)
      )

{-
Command

command    =  1*letter / 3digit

-}

type Command = ByteString

pCommand :: Parser Command
pCommand = (<?> "Command") $ takeWhile1 isLetter <|> BS.pack <$> ABS.count 3 pDigit

{-
Params

params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
           =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]
nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
                ; any octet except NUL, CR, LF, " " and ":"
middle     =  nospcrlfcl *( ":" / nospcrlfcl )
trailing   =  *( ":" / " " / nospcrlfcl )
-}

type Param = ByteString

pParams :: Parser [Param]
pParams = (<?> "Params") $ pRestOfParams 15
  where
    pRestOfParams :: Int -> Parser [Param]
    pRestOfParams maxParamsLeft =
      optional (pSpaces *> (isJust <$> optional pColon)) >>= \case
        Nothing -> return []
        Just parsedLeadingColon ->
          if parsedLeadingColon || maxParamsLeft == 1
            then maybeToList <$> optional (ABS.takeWhile1 (`BS.notElem` "\NUL\CR\LF"))
            else (:) <$> ABS.takeWhile (`BS.notElem` "\NUL\CR\LF ") <*> pRestOfParams (maxParamsLeft - 1)

{-
Spaces

SPACE      =  %x20        ; space character

-}

pSpaces :: Parser ByteString
pSpaces = (<?> "Spaces") $ ABS.takeWhile1 (== bSpace)

{-
Message Separator

crlf       =  %x0D %x0A   ; "carriage return" "linefeed"

* We loosen the requirement that both CR and LF should be there, because some implementations only send one or the other.  We also call this "message separator" since it's no longer strictly crlf.
-}

pMessageSeparator :: Parser ()
pMessageSeparator = (<?> "Message Separator") $ void $ pCR <* optional pLF <|> pLF

{-
Target

target     =  nickname / server

* We assume the grammar has a typo here, and "server" is actually a reference to the "servername" rule.
-}

data TTarget

tpTarget
  :: (NickName -> target)
  -> (ServerName -> target)
  -> TaggedParser TTarget target
tpTarget fromNickName fromServerName = Tagged . (<?> "Target") $ fromNickName <$> pNickName <|> fromServerName <$> pServerName

{-
MsgTarget

msgtarget  =  msgto *( "," msgto )
-}

data TMsgTarget

tpMsgTarget :: TaggedParser TMsgTarget msgTarget
tpMsgTarget = Tagged . (<?> "MsgTarget") $ undefined

{-
MsgTo

msgto      =  channel / ( user [ "%" host ] "@" servername )
msgto      =/ ( user "%" host ) / targetmask
msgto      =/ nickname / ( nickname "!" user "@" host )
-}

data TMsgTo

tpMsgTo :: TaggedParser TMsgTo msgTo
tpMsgTo = Tagged . (<?> "MsgTo") $ undefined

{-
Channel

channel    =  ( "#" / "+" / ( "!" channelid ) / "&" ) chanstring
              [ ":" chanstring ]
chanstring =  %x01-07 / %x08-09 / %x0B-0C / %x0E-1F / %x21-2B
chanstring =/ %x2D-39 / %x3B-FF
                ; any octet except NUL, BELL, CR, LF, " ", "," and ":"
channelid  = 5( %x41-5A / digit )   ; 5( A-Z / 0-9 )

* The grammar specifies that a channel name (i.e. chanstring) is only a single character.  This is obviously incorrect.
-}

type Channel = ByteString

pChannel :: Parser Channel
pChannel =
  (<?> "Channel") $
  (\p s1 s2 -> BS.concat [p, s1, s2]) <$>
  pChannelPrefix <*>
  pChannelString <*>
  option BS.empty (BS.cons <$> pColon <*> pChannelString)
  where
    pChannelPrefix :: Parser ByteString
    pChannelPrefix =
      BS.singleton <$> satisfy (`BS.elem` "#+&") <|>
      BS.cons <$> pExclam <*> pChannelId

    pChannelId :: Parser ByteString
    pChannelId = mfilter (BS.all $ (||) <$> isDigit <*> isUpperLetter) $ ABS.take 5

    pChannelString :: Parser ByteString
    pChannelString = ABS.takeWhile1 (`BS.notElem` "\NUL\BEL\CR\LF ,:")

{-
ServerName

servername =  hostname
-}

type ServerName = HostName

pServerName :: Parser ServerName
pServerName = pHostName

{-
Host

host       =  hostname / hostaddr
-}

type Host = ByteString

pHost :: Parser Host
pHost = (<?> "Host") $ pHostName <|> pHostAddr

{-
HostName

hostname   =  shortname *( "." shortname )
-}

type HostName = ByteString

pHostName :: Parser HostName
pHostName = (<?> "HostName") $ BS.concat .: (:) <$> pShortName <*> many (BS.cons <$> pPeriod <*> pShortName)

{-
ShortName

shortname  =  ( letter / digit ) *( letter / digit / "-" )
              *( letter / digit )
                ; as specified in RFC 1123 [HNAME]
-}

type ShortName = ByteString

pShortName :: Parser ShortName
pShortName =
  (<?> "ShortName") $
  BS.cons <$>
  ABS.satisfy isLetterOrDigit <*>
  ABS.takeWhile ((||) <$> isLetterOrDigit <*> (== bHyphen))

  where
    isLetterOrDigit = (||) <$> isLetter <*> isDigit

{-
HostAddr

hostaddr   =  ip4addr / ip6addr
-}

type HostAddr = ByteString

pHostAddr :: Parser HostAddr
pHostAddr = (<?> "HostAddr") $ pIP4Addr <|> pIP6Addr

{-
IP4Addr

ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit

* We omit the 3-digit limit for the sake of simplicity.  If this proves to be a problem, add this constraint to the logic.
-}

type IP4Addr = ByteString

pIP4Addr :: Parser IP4Addr
pIP4Addr =
  (<?> "IP4Addr") $
  BS.concat .: (:) <$>
  pDigits <*>
  ABS.count 3 (BS.cons <$> pPeriod <*> pDigits)

  where
    pDigits :: Parser ByteString
    pDigits = ABS.takeWhile1 isDigit

{-
IP6Addr

ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
-}

type IP6Addr = ByteString

pIP6Addr :: Parser IP6Addr
pIP6Addr = (<?> "IP6Addr") $ pStandardIP6Addr <|> pEncodedIP4Addr
  where
    pStandardIP6Addr :: Parser IP6Addr
    pStandardIP6Addr = BS.concat .: (:) <$> pHexDigits <*> ABS.count 7 (BS.cons <$> pColon <*> pHexDigits)

    pEncodedIP4Addr :: Parser IP6Addr
    pEncodedIP4Addr =
      (\s1 s2 ip4 -> BS.concat [s1, s2, ip4]) <$>
      ABS.string "0:0:0:0:0:" <*> (ABS.string "0:" <|> ABS.string "FFFF:") <*> pIP4Addr

    pHexDigits :: Parser ByteString
    pHexDigits = ABS.takeWhile1 isHexDigit

{-
NickName

nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
special    =  %x5B-60 / %x7B-7D
                 ; "[", "]", "\", "`", "\_", "\^", "{", "|", "}"

* We remove the length constraint, because real IRC servers allow much longer names.
-}

type NickName = ByteString

pNickName :: Parser NickName
pNickName =
  (<?> "NickName") $
  BS.cons <$>
  satisfy ((||) <$> isLetter <*> (`BS.elem` "[]\\`_^{|}")) <*>
  ABS.takeWhile (\b -> any ($ b) [isLetter, isDigit, (`BS.elem` "[]\\`_^{|}-")])

{-
TargetMask

targetmask =  ( "\$" / "#" ) mask
                ; see details on allowed masks in section 3.3.1

* The spec isn't very clear about the syntax of "mask".  TODO: Finish this...
-}

type TargetMask = ByteString

pTargetMask :: Parser TargetMask
pTargetMask = (<?> "TargetMask") $ BS.cons <$> satisfy (`BS.elem` "$#") <*> undefined

{-
User

user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
                ; any octet except NUL, CR, LF, " " and "@"
-}

type User = ByteString

pUser :: Parser User
pUser = (<?> "User") $ ABS.takeWhile1 (`BS.notElem` "\NUL\CR\LF @")

{-
Key

key        =  1*23( %x01-05 / %x07-08 / %x0C / %x0E-1F / %x21-7F )
                ; any 7-bit US\_ASCII character,
                ; except NUL, CR, LF, FF, h/v TABs, and " "

* We opt not to enforce the max-length constraint, since it's simpler and may end up not mattering.
* The comment indicates FF is excluded, but the spec itself includes that and excludes the ACK character (%x06).  Since the comment is more likely to express their intent, we include the ACK character, but exclude FF.
-}

type Key = ByteString

pKey :: Parser Key
pKey = (<?> "Key") $ ABS.takeWhile1 (`BS.notElem` "\NUL\CR\LF\FF\HT\VT ")

{-
letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
-}

isLetter :: Word8 -> Bool
isLetter = (||) <$> isUpperLetter <*> isLowerLetter

isUpperLetter :: Word8 -> Bool
isUpperLetter = inRange bUpperA bUpperZ

isLowerLetter :: Word8 -> Bool
isLowerLetter = inRange bLowerA bLowerZ

{-
digit      =  %x30-39                 ; 0-9
-}

isDigit :: Word8 -> Bool
isDigit = inRange bZero bNine

pDigit :: Parser Word8
pDigit = satisfy isDigit

{-
hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"
-}

isHexDigit :: Word8 -> Bool
isHexDigit = (||) <$> isDigit <*> inRange bUpperA bUpperF


{-
Old code:

pPrefix :: Parser Prefix
pPrefix = try (pPrefixNick <* pSpaces) <|> (pPrefixServerName <* pSpaces) <?> "Prefix"
  where
    pPrefixServerName = PrefixServerName <$> pHost
    pPrefixNick =
      PrefixNick <$>
      pNick <*>
      optional (pExclam *> pUser) <*>
      optional (pAt *> pHost)

pCommand :: Parser Command
pCommand = undefined

pSpaces :: Parser ()
pSpaces = void $ takeWhile1 (== _space)

pParams :: Parser [Param]
pParams = undefined

pParamMiddle :: Parser Param
pParamMiddle = undefined

pParamTrailing :: Parser Param
pParamTrailing = undefined

pEnd :: Parser ()
pEnd = void $ pCR <* optional pLF <|> pLF

pTarget :: Parser Target
pTarget = NE.fromList <$> pTo `sepBy1` pComma

pTo :: Parser To
pTo = undefined

pChannel :: Parser Channel
pChannel = BS.cons <$> satisfy (isIn "#&") <*> takeWhile1 isChannelChar

-- Based on <hname> from RFC 952
pHost :: Parser Host
pHost = BS.intercalate "." <$> pName `sepBy1` pPeriod
  where
    pName =
      BS.cons <$>
      satisfy isAlpha <*> (
        BS.concat <$>
        many (
          option id (BS.cons <$> pHyphen) <*>
          ABS.takeWhile1 isAlphaNum
        )
      )

pNick :: Parser Nick
pNick = takeWhile1 $ \b -> isAlphaNum b || isIn "-[]\\`^{}" b

pMask :: Parser Mask
pMask = BS.cons <$> satisfy (isIn "#$") <*> takeWhile1 isChannelChar

pUser :: Parser User
pUser = takeWhile1 $ not . isIn " \NUL\r\n"

isIn :: ByteString -> Word8 -> Bool
isIn bs = (`BS.any` bs) . (==)

isChannelChar :: Word8 -> Bool
isChannelChar = not . isIn " \b\NUL\r\n,"

[pSpace, pHyphen, pColon, pExclam, pPeriod, pComma, pAt, pCR, pLF] =
  word8 <$>
  [_space, _hyphen, _colon, _exclam, _period, _comma, _at, _cr, _lf]
-}
