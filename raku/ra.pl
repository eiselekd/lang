#!/usr/bin/rakudo

grammar REST
{
    token TOP { <slash><subject><slash><command>[<slash><data>]? }

    proto token command {*}
    token command:sym<create>   { <sym> }
    token command:sym<retrieve> { <sym> }
    token command:sym<update>   { <sym> }
    token command:sym<delete>   { <sym> }

    token subject { \w+ }
    token data    { .* }
    token slash   { \s* '/' \s* }
}

my @uris = ['/product/update/7/notify',
            '/product/create',
            '/item/delete/4'];

for @uris -> $uri {
    my $m = REST.parse($uri);
    say "Sub: " ~ $m<subject> ~ "Cmd: " ~ $m<command> ~ "Dat:" ~ $m<data>;
}
