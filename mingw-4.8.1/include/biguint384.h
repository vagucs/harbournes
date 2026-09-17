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
#ifndef _BIGUINT384_H_
#define _BIGUINT384_H_

#include "uint_types.h"
#define BIGUINT384_CELLS (384u / (8u * sizeof(UInt)))

/**
 Stores the value in array.
 Least significant item is at index 0,
 most significant item is at index (BIGUINT384_CELLS - 1)
*/
typedef struct {
 UInt dat[ BIGUINT384_CELLS ];
} BigUInt384;

/**
 Auxiliary type for the result of division (Storing quotient and remainder).
*/
typedef struct {
 BigUInt384 first;
 BigUInt384 second;
} BigUIntPair384;

// constructors
/**
 @brief Generates BigUInt384 instance initialized to 0.
*/
BigUInt384 biguint384_ctor_default();

/**
 @brief Generates BigUInt384 instance initialized to 1.
*/
BigUInt384 biguint384_ctor_unit();

/**
 @brief Generates BigUInt384 instance with the internal array taking its value from the dat array.
*/
BigUInt384 biguint384_ctor_standard(const UInt *dat);

/**
 @brief Generates a copy of orig.
 Actually, the bitwise copy also has the desired effect.
*/
BigUInt384 biguint384_ctor_copy(const BigUInt384 *orig);

/**
 @brief Generates BigUInt384 instance initialized to value.
*/
BigUInt384 biguint384_value_of_uint(UInt value);

/**
 @brief Value initialization from char array with hexadecimal digits.
 @param hex_digits Input character array.
 @param len Length of the input array.
*/
BigUInt384 biguint384_ctor_hexcstream(const char *hex_digits, buint_size_t len);

/**
 @brief Value initialization from char array with decimal digits.
 @param dec_digits Input character array.
 @param len Length of the input array.
*/
BigUInt384 biguint384_ctor_deccstream(const char *dec_digits, buint_size_t len);

/**
 @brief Signed value initialization from char array with decimal digits.
 @param dec_digits Input character array.
 @param len Length of the input array.
*/
BigUInt384 bigint384_ctor_deccstream(const char *dec_digits, buint_size_t len);

/**
 * @brief Import from byte array.
 * Overwrites the existing BigUInt384 instance.
 * @return Number of read bytes.
*/
buint_size_t biguint384_import(BigUInt384 *dest, const char *src);


// addition / subtraction
/**
 @brief Addition with overflow.
*/
BigUInt384 biguint384_add(const BigUInt384 *a, const BigUInt384 *b);

/**
 @brief Add-assignment with overflow.
*/
BigUInt384 *biguint384_add_assign(BigUInt384 *a, const BigUInt384 *b);

/**
 @brief Add-replacement with overflow.
 */
void biguint384_add_replace(BigUInt384 *dest, const BigUInt384 *a, const BigUInt384 *b);

/**
 @brief Add-replacement with in/out carry bit.
 */
void biguint384_adc_replace(BigUInt384 *dest, const BigUInt384 *a, const BigUInt384 *b, buint_bool *carry);

/**
 @brief Subtraction with underflow.
*/
BigUInt384 biguint384_sub(const BigUInt384 *a, const BigUInt384 *b);

/**
 @brief Sub-assignment with underflow.
*/
BigUInt384 *biguint384_sub_assign(BigUInt384 *a, const BigUInt384 *b);

/**
 @brief Sub-replacement with underflow.
 */
void biguint384_sub_replace(BigUInt384 *dest, const BigUInt384 *a, const BigUInt384 *b);

/**
 @brief Sub-replacement with in/out carry.
 */
void biguint384_sbc_replace(BigUInt384 *dest, const BigUInt384 *a, const BigUInt384 *b, buint_bool *carry);

// inc/dec
BigUInt384 *biguint384_inc(BigUInt384 *a);

BigUInt384 *biguint384_dec(BigUInt384 *a);


// shift operations
/**
 @brief Shift left operation.
*/
BigUInt384 biguint384_shl(const BigUInt384 *a, const buint_size_t shift);
BigUInt384 *biguint384_shl_or(BigUInt384 *dest, const BigUInt384 *a, const buint_size_t shift);
/**
 @brief Shift right operation.
*/
BigUInt384 biguint384_shr(const BigUInt384 *a, const buint_size_t shift);
BigUInt384 *biguint384_shr_assign(BigUInt384 *a, const buint_size_t shift);

/**
 @brief Rotate left operation.
*/
BigUInt384 biguint384_rol(const BigUInt384 *a, const buint_size_t shift);
/**
 @brief Rotate right operation.
*/
BigUInt384 biguint384_ror(const BigUInt384 *a, const buint_size_t shift);


// bitwise functions
/**
 @brief Bitwise AND operation.
*/
BigUInt384 biguint384_and(const BigUInt384 *a, const BigUInt384 *b);
/**
 @brief Bitwise OR operation.
*/
BigUInt384 biguint384_or(const BigUInt384 *a, const BigUInt384 *b);
/**
 @brief Bitwise NOT operation.
*/
BigUInt384 biguint384_not(const BigUInt384 *a);
/**
 @brief Bitwise XOR operation.
*/
BigUInt384 biguint384_xor(const BigUInt384 *a, const BigUInt384 *b);

// multiplication / division
/**
 @brief Multiplication of two numbers.
 @return Product of the factors (Least significant 384 bits).
*/
BigUInt384 biguint384_mul(const BigUInt384 *a, const BigUInt384 *b);

/**
 @brief Multiplication of two numbers resulting in double long (BigUIntPair384) value.
 @return Product of the factors (first: low 384 bits, second: high 384 bits).
*/
BigUIntPair384 biguint384_dmul(const BigUInt384 *a, const BigUInt384 *b);

/**
 @brief Division of a by b.
 @return First: quotient, second: remainder.
*/
BigUIntPair384 biguint384_div(const BigUInt384 *a, const BigUInt384 *b);

// comparison
/**
 @brief 'Less than' relation.
 @return Not zero: a is less than b. Zero: a is not less than b.
*/
buint_bool biguint384_lt(const BigUInt384 *a, const BigUInt384 *b);

/**
 @brief 'Less than' relation between signed values.
 @return Not zero: a is less than b. Zero: a is not less than b.
*/
buint_bool bigint384_lt(const BigUInt384 *a, const BigUInt384 *b);

/**
 @brief Checks equality.
 @return Not zero: a is equal to b. Zero: a is not equal to b.
*/
buint_bool biguint384_eq(const BigUInt384 *a, const BigUInt384 *b);

// misc.
/**
 @brief Most significant bit.
 @param a (Pointing to the) input value to examine.
 @return Index of the most significant bit set to 1.
*/
buint_size_t biguint384_msb(const BigUInt384 *a);
/**
 @brief Set a bit (to 1) of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
*/
void biguint384_sbit(BigUInt384 *a, buint_size_t bit);
/**
 @brief Clears a bit (set to 0) of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
*/
void biguint384_cbit(BigUInt384 *a, buint_size_t bit);
/**
 @brief Overwrite a bit of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
 @param value Set or clear the bit.
*/
void biguint384_obit(BigUInt384 *a, buint_size_t bit, buint_bool value);
/**
 @brief Get a bit of the value.
 @param a (Pointer to) the value to operate on.
 @param bit Index of the bit.
*/
buint_bool biguint384_gbit(const BigUInt384 *a, buint_size_t bit);

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
buint_size_t biguint384_print_hex(const BigUInt384 *a, char *buf, buint_size_t buf_len);
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
buint_size_t biguint384_print_dec(const BigUInt384 *a, char *buf, buint_size_t buf_len);

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
buint_size_t bigint384_print_dec(const BigUInt384 *a, char *buf, buint_size_t buf_len);

/**
 @brief Exports data into byte array.
 @return Number of written bytes.
*/
buint_size_t biguint384_export(const BigUInt384 *a, char *dest);

#endif

