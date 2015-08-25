# Generic Erlang CoAP Client/Server

Pure Erlang implementation of the Constrained Application Protocol (CoAP),
which aims to be conformant with:
 - CoAP core protocol [RFC 7252](https://tools.ietf.org/rfc/rfc7252.txt)
 - CoAP Observe option [draft-ietf-core-observe-16](https://tools.ietf.org/id/draft-ietf-core-observe-16.txt)
 - Block-wise transfers in CoAP [draft-ietf-core-block-17](https://tools.ietf.org/id/draft-ietf-core-block-17.txt)
 - CoRE link format [RFC 6690](https://tools.ietf.org/rfc/rfc6690.txt)

Tested with the following CoAP implementations:
 - C [libcoap](https://www.libcoap.net/)
 - [coap.me](http://coap.me/)

The API may change without notice and some functions may not be implemented.
Please [add an Issue](https://github.com/gotthardp/gen_coap/issues/new)
if you find a bug or miss a feature.


## Usage

### Client

Get the resource by:

    ```erlang
    {ok, content, Data} = coap_client:request(get, "coap://coap.me:5683")
    ```

### Server

Build and start the server:

    $ cd gen_coap
    $ make
    $ erl -pa ebin
    1> coap_server:start().

Invoke the client and access the server resources:

    $ ./coap-client coap://127.0.0.1/.well-known/core
