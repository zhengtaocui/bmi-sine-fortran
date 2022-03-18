#ifndef SERIALIZER_H
#define SERIALIZER_H


extern int serializer_factory(void **);
extern int serializer_destroy(void **);
extern int c_create_adapter(void **, void **);
extern int c_delete_adapter(void **);

extern int serialize(void **, void **, const char*);
extern int deserialize(void **, void **, const char*);
extern int compare(void **, void **, void **);

#endif //ifndef SERIALIZER_H
