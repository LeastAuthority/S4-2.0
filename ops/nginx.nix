# Tools for configuring Nginx.
{
  # Configure nginx to proxy from HTTP to HTTPS.
  cleartextToHTTPSProxy = pkgs: label: { proxyPass, listen, serverName }:
  { enable = true;
    appendConfig = ''
      error_log logs/error.log debug;
    '';
    virtualHosts =
    { ${label} =
      { inherit listen;
        inherit serverName;
        locations."/" =
        { # proxyPass gives the upstream address for the proxy.
          inherit proxyPass;
          extraConfig =
          ''
            # Modern TLS pretty much implies SNI.  And there is no good reason
            # not to try.
            proxy_ssl_server_name on;

            # It is almost pointless to do TLS without certificate
            # verification.  Turn this on.
            proxy_ssl_verify on;

            # nginx has a default verify depth of 1 instead of the more
            # typical 9.  depth of 1 is too low to verify the
            # leastauthority.com certificate.  raise the value back to
            # something more sensible.
            proxy_ssl_verify_depth 9;

            # Make sure that we can find the Nix certificate bundle.  This
            # option is required by nginx if proxy_ssl_verify is on (even if
            # the certificate bundle is at the OpenSSL default location).
            proxy_ssl_trusted_certificate /etc/ssl/certs/ca-bundle.crt;
          '';
        };
      };
    };
  };
}
