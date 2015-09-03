rotor
=====

An OTP application

Build
-----
```
varoun@ip-10-0-2-24:~/dev/rotor % rebar3 shell
===> Verifying dependencies...
===> Compiling rotor
Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V7.0.2  (abort with ^G)
1> resolver:start_link([{rotation, "www.bsd.org"}, {check_type, http}, {check_url, "/"}, {frequency, 30000}, {timeout, 2000}, {reals, ["www.freebsd.org", "www.netbsd.org", "www.openbsd.org", "www.gragonflybsd.org"]}]).
{ok,<0.78.0>}
2> resolver:gethostbyname("www.bsd.org").
ns_tryagain
3> resolver:gethostbyname("www.bsd.org").                                                                                                                                        ns_tryagain
4> resolver:gethostbyname("www.bsd.org").
{ns_success,"www.freebsd.org"}
5> resolver:gethostbyname("www.bsd.org").
{ns_success,"www.netbsd.org"}
6> resolver:gethostbyname("www.bsd.org").
{ns_success,"www.openbsd.org"}
7> resolver:gethostbyname("www.bsd.org").
{ns_success,"www.freebsd.org"}
8> resolver:gethostbyname("www.bsd.org").
{ns_success,"www.netbsd.org"}
9> resolver:gethostbyname("www.bsd.org").
{ns_success,"www.openbsd.org"}
10> resolver:gethostbyname("www.bsd.org").
{ns_success,"www.freebsd.org"}
11> resolver:gethostbyname("www.bsd.org").
{ns_success,"www.netbsd.org"}
12> resolver:gethostbyname("www.bsd.org").
{ns_success,"www.openbsd.org"}
13> resolver:gethostbyname("www.bsd.org").
{ns_success,"www.freebsd.org"}
14> q().
ok
15> varoun@ip-10-0-2-24:~/dev/rotor %

```
