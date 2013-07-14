#ifndef I2C_STUB
#define I2c_STUB

#define I2C_SLAVE 0x0 /* dummy I2C address */

#include <stdint.h>

static int16_t i2c_smbus_read_byte_data(int file, const uint8_t reg);
static int32_t i2c_smbus_read_word_data(int file, const uint8_t reg);

#endif
