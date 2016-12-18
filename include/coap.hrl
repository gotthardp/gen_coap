-define(DEFAULT_COAP_PORT, 5683).
-define(DEFAULT_COAPS_PORT, 5684).

-define(MAX_BLOCK_SIZE, 1024).
-define(DEFAULT_MAX_AGE, 60).

-record(coap_message, {type, method, id, token = <<>>, options = [], payload = <<>>}).
-record(coap_content, {etag, max_age = ?DEFAULT_MAX_AGE, format, payload = <<>>}).

-type coap_message() :: #coap_message{}.
-type coap_content() :: #coap_content{}.
