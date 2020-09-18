#define OSXSAVEFlag (1UL<<27)
#define AVXFlag     ((1UL<<28)|OSXSAVEFlag)
#define FMAFlag     ((1UL<<12)|AVXFlag|OSXSAVEFlag)
#define CLMULFlag   ((1UL<< 1)|AVXFlag|OSXSAVEFlag)
#define VAESFlag    ((1UL<<25)|AVXFlag|OSXSAVEFlag)



inline bool SimdDetectFeature(U32 idFeature)
{
	int EAX, EBX, ECX, EDX;
	cpuid(0, EAX, EBX, ECX, EDX);
	if((ECX & idFeature) != idFeature)
		return false;
	return true;
}
