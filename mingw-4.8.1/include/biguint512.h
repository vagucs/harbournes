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
#ifndef _BIGUINT512_H_
#define _BIGUINT512_H_

#include "uint_types.h"
#define BIGUINT512_CELLS (512u / (8u * sizeof(UInt)))

/**
 Stores the value in array.
 Least significant item is at index 0,
 most significant item is at index (BIGUINT512_CELLS - 1)
*/
typedef struct {
 UInt dat[ BIGUINT512_CELLS ];
} BigUInt512;

/**
 Auxiliary type for the result of division (Storing quotient and remainder).
*/
typedef struct {
 BigUInt512 first;
 BigUInt512 second;
} BigUIntPair512;

// constructors
/**
 @brief Generates BigUInt512 instance initialized to 0.
*/
BigUInt512 biguint512_ctor_default();

/**
 @brief Generates BigUInt512 instance initialized to 1.
*/
BigUInt512 biguint512_ctor_unit();

/**
 @brief Generates BigUInt512 instance with the internal array taking its value from the dat array.
*/
BigUInt512 biguint512_ctor_standard(const UInt *dat);

/**
 @brief Generates a copy of orig.
 Actually, the bitwise copy also has the desired effect.
*/
BigUInt512 biguint512_ctor_copy(const BigUInt512 *orig);

/**
 @brief Generates BigUInt512 instance initialized to value.
*/
BigUInt512 biguint512_value_of_uint(UInt value);

/**
 @brief Value initialization from char array with hexadecimal digits.
 @param hex_digits Input character array.
 @param len Length of the input array.
*/
BigUInt512 biguint512_ctor_hexcstream(const char *hex_digits, buint_size_t len);

/**
 @brief Value initialization from char array with decimal digits.
 @param dec_digits Input character array.
 @param len Length of the input array.
*/
BigUInt512 biguint512_ctor_deccstream(const char *dec_digits, buint_size_t len);

/**
 @brief Signed value initialization from char array with decimal digits.
 @param dec_digits Input character array.
 @param len Length of the input array.
*/
BigUInt512 bigint512_ctor_deccstream(const char *dec_digits, buint_size_t len);

/**
 * @brief Import from byte array.
 * Overwrites the existing BigUInt512 instance.
 * @return Number of read bytes.
*/
buint_size_t biguint512_import(BigUInt512 *dest, const char *src);


// addition / subtraction
/**
 @brief Addition with overflow.
*/
BigUInt512 biguint512_add(const BigUInt512 *a, const BigUInt512 *b);

/**
 @brief Add-assignment with overflow.
*/
BigUInt512 *biguint512_add_assign(BigUInt512 *a, const BigUInt512 *b);

/**
 @brief Add-replacement with overflow.
 */
void biguint512_add_replace(BigUInt512 *dest, const BigUInt512 *a, const BigUInt512 *b);

/**
 @brief Add-replacement with in/out carry bit.
 */
void biguint512_adc_replace(BigUInt512 *dest, const BigUInt512 *a, const BigUInt512 *b, buint_bool *carry);

/**
 @brief Subtraction with underflow.
*/
BigUInt512 biguint512_sub(const BigUInt512 *a, const BigUInt512 *b);

/**
 @brief Sub-assignment with underflow.
*/
BigUInt512 *biguint512_sub_assign(BigUInt512 *a, const BigUInt512 *b);

/**
 @brief Sub-replacement with underflow.
 */
void biguint512_sub_replace(BigUInt512 *dest, const BigUInt512 *a, const BigUInt512 *b);

/**
 @brief Sub-replacement with in/out carry.
 */
void biguint512_sbc_replace(BigUInt512 *dest, const BigUInt512 *a, const BigUInt512 *b, buint_bool *carry);

// inc/dec
BigUInt512 *biguint512_inc(BigUInt512 *a);

BigUInt512 *biguint512_dec(BigUInt512 *a);


// shift operations
/**
 @brief Shift left operation.
*/
BigUInt512 biguint512_shl(const BigUInt512 *a, const buint_size_t shift);
BigUInt512 *biguint512_shl_or(BigUInt512 *dest, const BigUInt512 *a, const buint_size_t shift);
/**
 @brief Shift right operation.
*/
BigUInt512 biguint512_shr(const BigUInt512 *a, const buint_size_t shift);
BigUInt512 *biguint512_shr_assign(BigUInt512 *a, const buint_size_t shift);

/**
 @brief Rotate left operation.
*/
BigUInt512 biguint512_rol(const BigUInt512 *a, const buint_size_t shift);
/**
 @brief Rotate right operation.
*/
BigUInt512 biguint512_ror(const BigUInt512 *a, const buint_size_t shift);


// bitwise functions
/**
 @brief Bitwise AND operation.
*/
BigUInt512 biguint512_and(const BigUInt512 *a, const BigUInt512 *b);
/**
 @brief Bitwise OR operation.
*/
BigUInt512 biguint512_or(const BigUInt512 *a, const BigUInt512 *b);
/**
 @brief Bitwise NOT operation.
*/
BigUInt512 biguint512_not(const BigUInt512 *a);
/**
 @brief Bitwise XOR operation.
*/
BigUInt512 biguint512_xor(const BigUInt512 *a, const BigUInt512 *b);

// multiplication / division
/**
 @brief Multiplication of two numbers.
 @return Product of the factors (Least significant 512 bits).
*/
BigUInt512 biguint512_mul(const BigUInt512 *a, const BigUInt512 *b);

/**
 @brief Multiplication of two numbers resulting in double long (BigUIntPair512) value.
 @return Product of the factors (first: low 512 bits, second: high 512 bits).
*/
BigUIntPair512 biguint512_dmul(const BigUInt512 *a, const BigUInt512 *b);

/**
 @brief Division of a by b.
 @return First: quotient, second: remainder.
*/
BigUIntPair512 biguint512_div(const BigUInt512 *a, const BigUInt512 *b);

// comparison
/**
 @brief 'Less than' relation.
 @return Not zero: a is less than b. Zero: a is not less than b.
*/
buint_bool biguint512_lt(const BigUInt512 *a, const BigUInt512 *b);

/**
 @brief 'Less than' relation between signed values.
 @return Not zero: a is less than b. Zero: a is not less than b.
*/
buint_bool bigint512_lt(const BigUInt512 *a, const BigUInt512 *b);

/**
 @brief Checks equality.
 @return Not zero: a is equal to b. Zero: a is not equal to b.
*/
buint_bool biguint512_eq(const BigUInt512 *a, const BigUInt512 *b);

// misc.
/**
 @brief Most significant bit.
 @param a (Pointing to the) input value to examine.
 @return Index of the most significant bit set to 1.
*/
buint_size_t biguint512_msb(const BigUInt512 *a);
/**
 @brief Set a bit (to 1) of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
*/
void biguint512_sbit(BigUInt512 *a, buint_size_t bit);
/**
 @brief Clears a bit (set to 0) of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
*/
void biguint512_cbit(BigUInt512 *a, buint_size_t bit);
/**
 @brief Overwrite a bit of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
 @param value Set or clear the bit.
*/
void biguint512_obit(BigUInt512 *a, buint_size_t bit, buint_bool value);
/**
 @brief Get a bit of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
*/
buint_bool biguint512_gbit(const BigUInt512 *a, buint_size_t bit);

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
buint_size_t biguint512_print_hex(const BigUInt512 *a, char *buf, buint_size_t buf_len);
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
buint_size_t biguint512_print_dec(const BigUInt512 *a, char *buf, buint_size_t buf_len);

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
buint_size_t bigint512_print_dec(const BigUInt512 *a, char *buf, buint_size_t buf_len);

/**
 @brief Exports data into byte array.
 @return Number of written bytes.
*/
buint_size_t biguint512_export(const BigUInt512 *a, char *dest);

#endif

