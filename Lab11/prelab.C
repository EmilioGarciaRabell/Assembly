#include “MKL05Z4.h”
typedef struct {
__IO uint32_t SC;
__IO uint32_t CNT;
__IO uint32_t MOD;
struct {
__IO uint32_t CnSC;
__IO uint32_t CnV;
} CONTROLS[6];
uint8_t RESERVED_0[20];
__IO uint32_t STATUS;
uint8_t RESERVED_1[48];
__IO uint32_t CONF;
} TPM_Type;
#define TPM0_BASE (0x40038000u)
#define TPM0 ((TPM_Type *)TPM0_BASE)


//TPM0_CONF
#define TPM_CONF_DEFAULT (0u)
//• TPM0_CNT
#define TPM_CNT_INIT (0u)
//• TPM0_MOD
#define TPM_MOD_PWM_PERIOD_20ms (60000u)
//• TPM0_C2SC
#define TPM_CnSC_PWMH (TPM_CnSC_MSB_MASK | \
TPM_CnSC_ELSB_MASK)
//• TPM0_C2V
/* value from PWM_duty_table */
//•TPM0_SC
#define TPM_SC_CMOD_CLK (1u)
#define TPM_SC_PS_DIV16 (0x4u)
#define TPM_SC_CLK_DIV16 \
((TPM_SC_CMOD_CLK << TPM_SC_CMOD_SHIFT) | \
TPM_SC_PS_DIV16)


/* DAC0_C0 symbol */
#define DAC_C0_ENABLE (DAC_C0_DACEN_MASK | DAC_C0_DACRFS_MASK)
/* DAC0_C1 symbol */
#define DAC_C1_BUFFER_DISABLED (0x00u)
/* DAC0_DAT symbols */
#define DAC_DATL_MIN (0x00u)
#define DAC_DATH_MIN (0x00u)

void Init_DAC(){
    // Initialize the KL05 DAC0 for for continuous 12 bit conversion
    //  of DAC0_DAT0 to an analog value of range
   /* Enable DAC0 module clock */
SIM->SCGC6 |= SIM_SCGC6_DAC0_MASK;
/* Set DAC0 DMA disabled and buffer disabled */
DAC0->C1 = DAC_C1_BUFFER_DISABLED;
/* Set DAC0 enabled with VDDA as reference voltage */
/* and read pointer interrupts disabled */
DAC0->C0 = DAC_C0_ENABLE;
/* Set DAC0 output voltage at minimum value */
DAC0->DAT[0].DATL = DAC_DATL_MIN;
    DAC0->DAT[0].DATH = DAC_DATH_MIN;


}

void Init_And_Cal_ADC0(){

}

void Init_TPM0(){

}