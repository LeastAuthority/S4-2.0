# Tor-related helpers.
let
  torBool = x: if x then "1" else "0";
in
{
  # Construct an extra Tor configuration blob to insert into the Tor
  # configuration file.
  extraConfig =
  { cookieAuthentication, hiddenServiceSingleHopMode, hiddenServiceNonAnonymousMode, socksPort }:
  ''
  CookieAuthentication ${torBool cookieAuthentication}
  HiddenServiceSingleHopMode ${torBool hiddenServiceSingleHopMode}
  HiddenServiceNonAnonymousMode ${torBool hiddenServiceNonAnonymousMode}
  SocksPort ${toString socksPort}
  '';
}
