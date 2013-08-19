Erlang HTTP RPC
===============

Example Setting:

```erlang
{ehrpc, [
    {client, [
        {services, [
            {yfm1, [
                {url, "http://localhost:5566"}
              ]}
          ]}
      ]},
    {server, [
        {host, "127.0.0.1"},
        {port, 5566},
        {middlewares, [ehrpc_mfa]},
        {size, 5}
      ]}
  ]}
```