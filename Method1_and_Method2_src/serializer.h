#ifndef SERIALIZER_H
#define SERIALIZER_H


extern int get_serializer_handle(void **);
extern int delete_serializer_handle(void **);
extern int get_serializer_box(void **, void **);
extern int delete_serializer_box(void **);
extern int register_serializer(void **);
extern int unregister_serializer(void **);

extern int serialize(void **, void **, const char*);
extern int deserialize(void **, void **, const char*);
extern int compare(void **, void **, void **);

#endif //ifndef SERIALIZER_H
