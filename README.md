rotor
=====

An OTP application

Build
-----

	varoun@ip-10-0-2-24:~/dev/rotor % rebar3 compile
	===> Verifying dependencies...
        ===> Compiling rotor
        varoun@ip-10-0-2-24:~/dev/rotor % rebar3 shell
        ===> Verifying dependencies...
        ===> Compiling rotor
        Erlang/OTP 18 [erts-7.0.2] [source] [64-bit] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

        Eshell V7.0.2  (abort with ^G)
        1> check_rotation:start_link([{rotation, "www.bsd.org"}, {check_type, http}, {check_url, "/"}, {frequency, 10}, {timeout, 2000}, {reals, ["www.freebsd.org", "www.netbsd.org", "www.openbsd.org", "www.gragonflybsd.org"]}]).
        {ok,<0.80.0>}
        2> check_rotation:check("www.bsd.org").
        [[{real,"www.freebsd.org"},{status,healthy}],
         [{real,"www.netbsd.org"},{status,healthy}],
         [{real,"www.openbsd.org"},{status,healthy}],
         [{real,"www.gragonflybsd.org"},
          {status,{unhealthy,{failed_connect,[{to_address,{"www.gragonflybsd.org",
                                                   80}},
                                              {inet,[inet],nxdomain}]}}}]]
        3>

