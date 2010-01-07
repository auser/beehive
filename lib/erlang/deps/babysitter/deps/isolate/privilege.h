
void drop_privilege_temporarily(uid_t new_uid) throw (runtime_error);
void drop_privilege_permanently(uid_t new_uid) throw (runtime_error);
void restore_privilege() throw (runtime_error);

