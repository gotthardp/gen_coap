# Generic Erlang CoAP Client/Server

Pure Erlang implementation of the Constrained Application Protocol (CoAP) conformant to:
 - CoAP core protocol [RFC 7252](https://tools.ietf.org/rfc/rfc7252.txt)
 - CoAP Observe option [draft-ietf-core-observe-16](https://tools.ietf.org/id/draft-ietf-core-observe-16.txt)
 - CoRE link format [RFC 6690](https://tools.ietf.org/rfc/rfc6690.txt)

Tested with the following CoAP implementations:
 - C [libcoap](https://www.libcoap.net/)

**This is under development. The API may change without notice.**

## Manual testing

    $ cd gen_coap
    $ make
    $ erl -pa ebin
    1> gen_coap:start().

    $ ./coap-client coap://127.0.0.1/.well-known/core
