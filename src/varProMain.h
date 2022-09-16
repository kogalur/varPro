#ifndef  RF_VAR_PRO_MAIN_H
#define  RF_VAR_PRO_MAIN_H
void varProMain(char mode, int seedValue);
void complement(uint    originalMemberSize,
                uint   *originalMembers,
                uint    releasedMemberSize,
                uint   *releasedMembers,
                uint   *complementMembers);
void test();
#endif
