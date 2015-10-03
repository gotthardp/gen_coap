-define(MAX_BLOCK_SIZE, 1024).

-record(coap_message, {type, method, id, token = <<>>, options = [], payload = <<>>}).

-record(coap_content, {etag, format, payload = <<>>}).

-type coap_message() :: #coap_message{}.
