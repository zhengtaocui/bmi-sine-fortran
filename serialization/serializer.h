#ifndef SERIALIZER_H
#define SERIALIZER_H


extern int bmi_factory(void **);
extern int bmi_destroy(void **);

extern int c_create_box(void **, void **);
extern int c_delete_box(void **);

extern int serializer_factory(void **);
extern int serializer_destroy(void **);
extern int c_create_adapter(void **, void **);
extern int c_delete_adapter(void **);

extern int initialize(void **, const char*);
extern int get_component_name(void **, char*);
extern int get_start_time(void **, double* );
extern int get_end_time(void **, double* );
extern int get_current_time(void **, double* );
extern int update(void **);
extern int finalize(void **);

extern int serialize(void **, void **, const char*);
extern int deserialize(void **, void **, const char*);
extern int compare(void **, void **, void **);

#endif //ifndef SERIALIZER_H
