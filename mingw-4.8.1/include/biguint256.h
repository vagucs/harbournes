/*****************************************************************************

    Copyright 2020 SZIGETI János

    This file is part of biguint library (Big Unsigned Integers).

    Biguint is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Biguint is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*****************************************************************************/
#ifndef _BIGUINT256_H_
#define _BIGUINT256_H_

#include "uint_types.h"
#define BIGUINT256_CELLS (256u / (8u * sizeof(UInt)))

/**
 Stores the value in array.
 Least significant item is at index 0,
 most significant item is at index (BIGUINT256_CELLS - 1)
*/
typedef struct {
 UInt dat[ BIGUINT256_CELLS ];
} BigUInt256;

/**
 Auxiliary type for the result of division (Storing quotient and remainder).
*/
typedef struct {
 BigUInt256 first;
 BigUInt256 second;
} BigUIntPair256;

// constructors
/**
 @brief Generates BigUInt256 instance initialized to 0.
*/
BigUInt256 biguint256_ctor_default();

/**
 @brief Generates BigUInt256 instance initialized to 1.
*/
BigUInt256 biguint256_ctor_unit();

/**
 @brief Generates BigUInt256 instance with the internal array taking its value from the dat array.
*/
BigUInt256 biguint256_ctor_standard(const UInt *dat);

/**
 @brief Generates a copy of orig.
 Actually, the bitwise copy also has the desired effect.
*/
BigUInt256 biguint256_ctor_copy(const BigUInt256 *orig);

/**
 @brief Generates BigUInt256 instance initialized to value.
*/
BigUInt256 biguint256_value_of_uint(UInt value);

/**
 @brief Value initialization from char array with hexadecimal digits.
 @param hex_digits Input character array.
 @param len Length of the input array.
*/
BigUInt256 biguint256_ctor_hexcstream(const char *hex_digits, buint_size_t len);

/**
 @brief Value initialization from char array with decimal digits.
 @param dec_digits Input character array.
 @param len Length of the input array.
*/
BigUInt256 biguint256_ctor_deccstream(const char *dec_digits, buint_size_t len);

/**
 @brief Signed value initialization from char array with decimal digits.
 @param dec_digits Input character array.
 @param len Length of the input array.
*/
BigUInt256 bigint256_ctor_deccstream(const char *dec_digits, buint_size_t len);

/**
 * @brief Import from byte array.
 * Overwrites the existing BigUInt256 instance.
 * @return Number of read bytes.
*/
buint_size_t biguint256_import(BigUInt256 *dest, const char *src);


// addition / subtraction
/**
 @brief Addition with overflow.
*/
BigUInt256 biguint256_add(const BigUInt256 *a, const BigUInt256 *b);

/**
 @brief Add-assignment with overflow.
*/
BigUInt256 *biguint256_add_assign(BigUInt256 *a, const BigUInt256 *b);

/**
 @brief Add-replacement with overflow.
 */
void biguint256_add_replace(BigUInt256 *dest, const BigUInt256 *a, const BigUInt256 *b);

/**
 @brief Add-replacement with in/out carry bit.
 */
void biguint256_adc_replace(BigUInt256 *dest, const BigUInt256 *a, const BigUInt256 *b, buint_bool *carry);

/**
 @brief Subtraction with underflow.
*/
BigUInt256 biguint256_sub(const BigUInt256 *a, const BigUInt256 *b);

/**
 @brief Sub-assignment with underflow.
*/
BigUInt256 *biguint256_sub_assign(BigUInt256 *a, const BigUInt256 *b);

/**
 @brief Sub-replacement with underflow.
 */
void biguint256_sub_replace(BigUInt256 *dest, const BigUInt256 *a, const BigUInt256 *b);

/**
 @brief Sub-replacement with in/out carry.
 */
void biguint256_sbc_replace(BigUInt256 *dest, const BigUInt256 *a, const BigUInt256 *b, buint_bool *carry);

// inc/dec
BigUInt256 *biguint256_inc(BigUInt256 *a);

BigUInt256 *biguint256_dec(BigUInt256 *a);


// shift operations
/**
 @brief Shift left operation.
*/
BigUInt256 biguint256_shl(const BigUInt256 *a, const buint_size_t shift);
BigUInt256 *biguint256_shl_or(BigUInt256 *dest, const BigUInt256 *a, const buint_size_t shift);
/**
 @brief Shift right operation.
*/
BigUInt256 biguint256_shr(const BigUInt256 *a, const buint_size_t shift);
BigUInt256 *biguint256_shr_assign(BigUInt256 *a, const buint_size_t shift);

/**
 @brief Rotate left operation.
*/
BigUInt256 biguint256_rol(const BigUInt256 *a, const buint_size_t shift);
/**
 @brief Rotate right operation.
*/
BigUInt256 biguint256_ror(const BigUInt256 *a, const buint_size_t shift);


// bitwise functions
/**
 @brief Bitwise AND operation.
*/
BigUInt256 biguint256_and(const BigUInt256 *a, const BigUInt256 *b);
/**
 @brief Bitwise OR operation.
*/
BigUInt256 biguint256_or(const BigUInt256 *a, const BigUInt256 *b);
/**
 @brief Bitwise NOT operation.
*/
BigUInt256 biguint256_not(const BigUInt256 *a);
/**
 @brief Bitwise XOR operation.
*/
BigUInt256 biguint256_xor(const BigUInt256 *a, const BigUInt256 *b);

// multiplication / division
/**
 @brief Multiplication of two numbers.
 @return Product of the factors (Least significant 256 bits).
*/
BigUInt256 biguint256_mul(const BigUInt256 *a, const BigUInt256 *b);

/**
 @brief Multiplication of two numbers resulting in double long (BigUIntPair256) value.
 @return Product of the factors (first: low 256 bits, second: high 256 bits).
*/
BigUIntPair256 biguint256_dmul(const BigUInt256 *a, const BigUInt256 *b);

/**
 @brief Division of a by b.
 @return First: quotient, second: remainder.
*/
BigUIntPair256 biguint256_div(const BigUInt256 *a, const BigUInt256 *b);

// comparison
/**
 @brief 'Less than' relation.
 @return Not zero: a is less than b. Zero: a is not less than b.
*/
buint_bool biguint256_lt(const BigUInt256 *a, const BigUInt256 *b);

/**
 @brief 'Less than' relation between signed values.
 @return Not zero: a is less than b. Zero: a is not less than b.
*/
buint_bool bigint256_lt(const BigUInt256 *a, const BigUInt256 *b);

/**
 @brief Checks equality.
 @return Not zero: a is equal to b. Zero: a is not equal to b.
*/
buint_bool biguint256_eq(const BigUInt256 *a, const BigUInt256 *b);

// misc.
/**
 @brief Most significant bit.
 @param a (Pointing to the) input value to examine.
 @return Index of the most significant bit set to 1.
*/
buint_size_t biguint256_msb(const BigUInt256 *a);
/**
 @brief Set a bit (to 1) of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
*/
void biguint256_sbit(BigUInt256 *a, buint_size_t bit);
/**
 @brief Clears a bit (set to 0) of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
*/
void biguint256_cbit(BigUInt256 *a, buint_size_t bit);
/**
 @brief Overwrite a bit of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
 @param value Set or clear the bit.
*/
void biguint256_obit(BigUInt256 *a, buint_size_t bit, buint_bool value);
/**
 @brief Get a bit of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
*/
buint_bool biguint256_gbit(const BigUInt256 *a, buint_size_t bit);

// out
/**
 @brief Export the value in character array format, base 16.
 Digits greater than 9 are written in upper case.
 The method does not write terminating 0 character.
 However, the method returns where the exported data terminates
 (where to put terminating 0 if the caller wants to treat to character array as a C-string).
 @param a Pointer to the value to export.
 @param buf Target of the export.
 @param buf_len Length of the target buffer.
 @return Length of the written characters. Zero: buf_len is to small to store the value.
*/
buint_size_t biguint256_print_hex(const BigUInt256 *a, char *buf, buint_size_t buf_len);
/**
 @brief Export the value in character array format, base 10.
 The method does not write terminating 0 character.
 However, the method returns where the exported data terminates
 (where to put terminating 0 if the caller wants to treat to character array as a C-string).
 @param a Pointer to the value to export.
 @param buf Target of the export.
 @param buf_len Length of the target buffer.
 @return Length of the written characters. Zero: buf_len is to small to store the value.
*/
buint_size_t biguint256_print_dec(const BigUInt256 *a, char *buf, buint_size_t buf_len);

/**
 @brief Export the value treated as signed in character array format, base 10.
 The method does not write terminating 0 character.
 However, the method returns where the exported data terminates
 (where to put terminating 0 if the caller wants to treat to character array as a C-string).
 @param a Pointer to the value to export.
 @param buf Target of the export.
 @param buf_len Length of the target buffer.
 @return Length of the written characters. Zero: buf_len is to small to store the value.
*/
buint_size_t bigint256_print_dec(const BigUInt256 *a, char *buf, buint_size_t buf_len);

/**
 @brief Exports data into byte array.
 @return Number of written bytes.
*/
buint_size_t biguint256_export(const BigUInt256 *a, char *dest);

#endif

