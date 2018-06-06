#include <all.h>
#include <sys/msg.h>
#include <errno.h>

B building_control_gen_periodic_MessageQueue_sendAsync(StackFrame caller, Z msgid, Z port, art_DataContent d) {
  struct Message m = { .mtype = port, .data = *d };
  int status = msgsnd(msgget((key_t) msgid, 0644), &m, sizeof(union art_DataContent), IPC_NOWAIT);
  if (status == -1) {
    errno = 0;
    return F;
  }
  return T;
}

void building_control_gen_periodic_MessageQueue_receiveAsync(Option_02FA6D result, StackFrame caller) {
  struct Message r;
  ssize_t status = msgrcv(msqid, &r, sizeof(union art_DataContent), 0, 0);
  if (status == (ssize_t) - 1) {
    result->type = TNone_93AA2B;
    errno = 0;
  } else {
    result->type = TSome_E9D1E5;
    result->Some_E9D1E5.value.type = TTuple2_D0E3BB;
    result->Some_E9D1E5.value._1 = (Z) r.mtype;
    Type_assign(&(result->Some_E9D1E5.value._2), &r.data, sizeOf((Type) &r.data));
  }
}
