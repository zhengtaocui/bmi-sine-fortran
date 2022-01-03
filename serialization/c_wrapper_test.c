#include <stdio.h>
#include <msgpack.h>

int c_wrapper_test_(int* cint, char* cc1, char* cc, int cl1, int cl)
{
    cc[cl--] = '\0'; //NULL terminate the string
    printf( "inside c_wrapper_test: %s, testint = %d, cl = %d, cc1 = %s, cl1 = %d\n", cc, *cint, cl, cc1, cl1 );
    //printf( "inside c_wrapper_test: %s, testint = %d, cl = %d\n", cc, 10, cl );
    //
        /* creates buffer and serializer instance. */
        msgpack_sbuffer* buffer = msgpack_sbuffer_new();
            msgpack_packer* pk = msgpack_packer_new(buffer, msgpack_sbuffer_write);

        /* serializes ["Hello", "MessagePack"]. */
        msgpack_pack_array(pk, 2);
        msgpack_pack_bin(pk, 5);
        msgpack_pack_bin_body(pk, "Hello", 5);
        msgpack_pack_bin(pk, 11);
        msgpack_pack_bin_body(pk, "MessagePack", 11);

        /* deserializes it. */
        msgpack_unpacked msg;
        msgpack_unpacked_init(&msg);
        msgpack_unpack_return ret = msgpack_unpack_next(&msg, buffer->data, buffer->size, NULL);

        /* prints the deserialized object. */
        msgpack_object obj = msg.data;
        msgpack_object_print(stdout, obj);  /*=> ["Hello", "MessagePack"] */

        /* cleaning */
        msgpack_unpacked_destroy(&msg);
        msgpack_sbuffer_free(buffer);
        msgpack_packer_free(pk);	
    return( 0 );
}
