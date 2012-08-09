! bof                                                                           ! environ.fpp: 1
! **********************************************************************        ! environ.fpp: 2
! Fortran 95 module environment                                                 ! environ.fpp: 3
                                                                                ! environ.fpp: 4
! **********************************************************************        ! environ.fpp: 5
! Source Control Strings                                                        ! environ.fpp: 6
                                                                                ! environ.fpp: 7
! $Id: environ.fpp 1.4 2003/10/03 19:35:55Z Dan Release $                       ! environ.fpp: 8
                                                                                ! environ.fpp: 9
! **********************************************************************        ! environ.fpp: 10
!  copyright 2003 Purple Sage Computing Solutions, Inc.                         ! environ.fpp: 11
                                                                                ! environ.fpp: 12
! **********************************************************************        ! environ.fpp: 13
! some unix f77 subprograms                                                     ! environ.fpp: 14
                                                                                ! environ.fpp: 15
! **********************************************************************        ! environ.fpp: 16
! Summary of License                                                            ! environ.fpp: 17
                                                                                ! environ.fpp: 18
!   This library is free software; you can redistribute it and/or               ! environ.fpp: 19
!   modify it under the terms of the GNU Library General Public                 ! environ.fpp: 20
!   License as published by the Free Software Foundation; either                ! environ.fpp: 21
!   version 2 of the License, or (at your option) any later version.            ! environ.fpp: 22
                                                                                ! environ.fpp: 23
!   This library is distributed in the hope that it will be useful,             ! environ.fpp: 24
!   but WITHOUT ANY WARRANTY; without even the implied warranty of              ! environ.fpp: 25
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           ! environ.fpp: 26
!   Library General Public License for more details.                            ! environ.fpp: 27
                                                                                ! environ.fpp: 28
!   You should have received a copy of the GNU Library General Public           ! environ.fpp: 29
!   License along with this library; if not, write to the Free                  ! environ.fpp: 30
!   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.          ! environ.fpp: 31
                                                                                ! environ.fpp: 32
! To report bugs, suggest enhancements, etc. to the Authors,                    ! environ.fpp: 33
! Contact:                                                                      ! environ.fpp: 34
!    Purple Sage Computing Solutions, Inc.                                      ! environ.fpp: 35
!                               send email to dnagle@erols.com                  ! environ.fpp: 36
!                                   or fax to 703 471 0684 (USA)                ! environ.fpp: 37
!                                  or mail to 12142 Purple Sage Ct.             ! environ.fpp: 38
!                                             Reston, VA 20194-5621 USA         ! environ.fpp: 39
                                                                                ! environ.fpp: 40
! **********************************************************************        ! environ.fpp: 41
! use standard_types                                                            ! environ.fpp: 42
                                                                                ! environ.fpp: 43
! **********************************************************************        ! environ.fpp: 44
                                                                                ! environ.fpp: 45
! environment constants                                                         ! environ.fpp: 46
                                                                                ! environ.fpp: 47
!   unknown_option= character returned by getopt meaning 'unknown option'       ! environ.fpp: 48
!   end_of_options= integer getopt return value meaning 'no more words'         ! environ.fpp: 49
                                                                                ! environ.fpp: 50
! environment variables                                                         ! environ.fpp: 51
                                                                                ! environ.fpp: 52
!   optarg= character getopt returns option string                              ! environ.fpp: 53
!   optind= integer pointing to next word for getopt to process                 ! environ.fpp: 54
                                                                                ! environ.fpp: 55
! environment library                                                           ! environ.fpp: 56
                                                                                ! environ.fpp: 57
!   abort( code)                                                                ! environ.fpp: 58
                                                                                ! environ.fpp: 59
!   assert( expression, string)                                                 ! environ.fpp: 60
                                                                                ! environ.fpp: 61
!   getopt( optstring) sets optarg, optind                                      ! environ.fpp: 62
                                                                                ! environ.fpp: 63
!   lshift() left and                                                           ! environ.fpp: 64
!   rshift() right shifts                                                       ! environ.fpp: 65
                                                                                ! environ.fpp: 66
!   cut( string, sep, nsubs, substrings, subslengths, stat)                     ! environ.fpp: 67
                                                                                ! environ.fpp: 68
!   swab() copy words swapping even and odd bytes                               ! environ.fpp: 69
                                                                                ! environ.fpp: 70
                                                                                ! environ.fpp: 72
                                                                                ! environ.fpp: 74
                                                                                ! environ.fpp: 76
                                                                                ! environ.fpp: 78
! **********************************************************************        ! environ.fpp: 79
                                                                                ! environ.fpp: 80
! environment                                                                   ! environ.fpp: 81
                                                                                ! environ.fpp: 82
! **********************************************************************        ! environ.fpp: 83
                                                                                ! environ.fpp: 84
module environment                                                              ! environ.fpp: 85
                                                                                ! environ.fpp: 86
! **********************************************************************        ! environ.fpp: 87
                                                                                ! environ.fpp: 88
! use standard processor description                                            ! environ.fpp: 89
                                                                                ! environ.fpp: 90
use standard_types                                                              ! environ.fpp: 91
                                                                                ! environ.fpp: 92
! **********************************************************************        ! environ.fpp: 93
                                                                                ! environ.fpp: 94
! explicit declarations                                                         ! environ.fpp: 95
                                                                                ! environ.fpp: 96
implicit none                                                        ! no implicit typing ! environ.fpp: 97
                                                                                          ! environ.fpp: 98
! **********************************************************************                  ! environ.fpp: 99
                                                                                          ! environ.fpp: 100
! explicit export                                                                         ! environ.fpp: 101
                                                                                          ! environ.fpp: 102
private                                                              ! no implicit exporting ! environ.fpp: 103
                                                                                             ! environ.fpp: 104
! **********************************************************************                     ! environ.fpp: 105
                                                                                             ! environ.fpp: 106
!  RCS strings                                                                               ! environ.fpp: 107
                                                                                             ! environ.fpp: 108
! **********************************************************************                     ! environ.fpp: 109
                                                                                             ! environ.fpp: 110
character( len= *), public, parameter :: environment_rcs_id = &                              ! environ.fpp: 111
      '$Id: environ.fpp 1.4 2003/10/03 19:35:55Z Dan Release $'                              ! environ.fpp: 112
                                                                                             ! environ.fpp: 113
! **********************************************************************                     ! environ.fpp: 114
                                                                                             ! environ.fpp: 115
!  communication with getopt()                                                               ! environ.fpp: 116
                                                                                             ! environ.fpp: 117
! ----------------------------------------------------------------------                     ! environ.fpp: 118
                                                                                             ! environ.fpp: 119
!  getopt() 'no more arguments'                                                              ! environ.fpp: 120
                                                                                             ! environ.fpp: 121
integer, public, parameter :: end_of_args = -1                                               ! environ.fpp: 122
                                                                                             ! environ.fpp: 123
!  getopt() 'not in optltrs'                                                                 ! environ.fpp: 124
                                                                                             ! environ.fpp: 125
character( len= 1), public, parameter :: unknown_option = '?'                                ! environ.fpp: 126
                                                                                             ! environ.fpp: 127
!  getopt() out string                                                                       ! environ.fpp: 128
                                                                                             ! environ.fpp: 129
integer, public, parameter :: optarg_len = 1024                                              ! environ.fpp: 130
                                                                                             ! environ.fpp: 131
character( len= optarg_len), public, save :: optarg = unknown_option                         ! environ.fpp: 132
                                                                                             ! environ.fpp: 133
!  getopt() out index                                                                        ! environ.fpp: 134
                                                                                             ! environ.fpp: 135
integer, public, save :: optind = 0                                                          ! environ.fpp: 136
                                                                                             ! environ.fpp: 137
! **********************************************************************                     ! environ.fpp: 138
                                                                                             ! environ.fpp: 139
!  library                                                                                   ! environ.fpp: 140
                                                                                             ! environ.fpp: 141
! **********************************************************************                     ! environ.fpp: 142
                                                                                             ! environ.fpp: 143
!  unix f77 shift routines                                                                   ! environ.fpp: 144
                                                                                             ! environ.fpp: 145
public :: lshift                                                     ! use generic           ! environ.fpp: 146
                                                                                             ! environ.fpp: 147
interface lshift                                                     ! generic name          ! environ.fpp: 148
   module procedure byte_lshift                                                              ! environ.fpp: 150
   module procedure short_lshift                                                             ! environ.fpp: 153
   module procedure int_lshift                                                               ! environ.fpp: 156
   module procedure long_lshift                                                              ! environ.fpp: 159
end interface                                                                                ! environ.fpp: 161
                                                                                             ! environ.fpp: 162
public :: rshift                                                     ! use generic           ! environ.fpp: 163
                                                                                             ! environ.fpp: 164
interface rshift                                                     ! generic name          ! environ.fpp: 165
   module procedure byte_rshift                                                              ! environ.fpp: 167
   module procedure short_rshift                                                             ! environ.fpp: 170
   module procedure int_rshift                                                               ! environ.fpp: 173
   module procedure long_rshift                                                              ! environ.fpp: 176
end interface                                                                                ! environ.fpp: 178
                                                                                             ! environ.fpp: 179
! **********************************************************************                     ! environ.fpp: 180
                                                                                             ! environ.fpp: 181
!  module procedures                                                                         ! environ.fpp: 182
                                                                                             ! environ.fpp: 183
public :: abort                                                                              ! environ.fpp: 184
public :: assert                                                                             ! environ.fpp: 185
                                                                                             ! environ.fpp: 186
public :: getopt                                                                             ! environ.fpp: 187
                                                                                             ! environ.fpp: 188
public :: cut                                                                                ! environ.fpp: 189
public :: swab                                                                               ! environ.fpp: 190
                                                                                             ! environ.fpp: 191
! **********************************************************************                     ! environ.fpp: 192
                                                                                             ! environ.fpp: 193
!  library                                                                                   ! environ.fpp: 194
                                                                                             ! environ.fpp: 195
contains                                                             ! environ               ! environ.fpp: 196
                                                                                             ! environ.fpp: 197
! **********************************************************************                     ! environ.fpp: 198
                                                                                             ! environ.fpp: 199
!  public abort: terminate program with nonzero exit status                                  ! environ.fpp: 200
                                                                                             ! environ.fpp: 201
subroutine abort( code)                                                                      ! environ.fpp: 202
                                                                                             ! environ.fpp: 203
integer, intent( in), optional :: code                                                       ! environ.fpp: 204
                                                                                             ! environ.fpp: 205
!  local                                                                                     ! environ.fpp: 206
                                                                                             ! environ.fpp: 207
   integer :: icode                                                                          ! environ.fpp: 208
                                                                                             ! environ.fpp: 209
!  abort() text                                                                              ! environ.fpp: 210
                                                                                             ! environ.fpp: 211
continue                                                             ! abort()               ! environ.fpp: 212
                                                                                             ! environ.fpp: 213
   default_code: if( present( code) )then                            ! if called with code   ! environ.fpp: 214
                                                                                             ! environ.fpp: 215
      icode = code                                                                           ! environ.fpp: 216
                                                                                             ! environ.fpp: 217
   else default_code                                                 ! else use default      ! environ.fpp: 218
                                                                                             ! environ.fpp: 219
      icode = 1                                                                              ! environ.fpp: 220
                                                                                             ! environ.fpp: 221
   endif default_code                                                                        ! environ.fpp: 222
                                                                                             ! environ.fpp: 223
   write( unit= error_unit, fmt= *) ' abort: ', icode                ! complain              ! environ.fpp: 224
                                                                                             ! environ.fpp: 225
stop 'abort'                                                         ! quit status code or 1 ! environ.fpp: 226
                                                                                             ! environ.fpp: 227
!  abort()                                                                                   ! environ.fpp: 228
                                                                                             ! environ.fpp: 229
end subroutine abort                                                                         ! environ.fpp: 230
                                                                                             ! environ.fpp: 231
! **********************************************************************                     ! environ.fpp: 232
                                                                                             ! environ.fpp: 233
!  public assert: verify assertion                                                           ! environ.fpp: 234
                                                                                             ! environ.fpp: 235
subroutine assert( expression, string)                                                       ! environ.fpp: 236
                                                                                             ! environ.fpp: 237
logical, intent( in) :: expression                                                           ! environ.fpp: 238
                                                                                             ! environ.fpp: 239
character( len= *), intent( in) :: string                                                    ! environ.fpp: 240
                                                                                             ! environ.fpp: 241
!  assert() text                                                                             ! environ.fpp: 242
                                                                                             ! environ.fpp: 243
continue                                                             ! assert()              ! environ.fpp: 244
                                                                                             ! environ.fpp: 245
   assertion_fails: if( .not. expression )then                       ! expression false      ! environ.fpp: 246
                                                                                             ! environ.fpp: 247
      write( unit= error_unit, fmt= *) ' assertion failed: ' // string                       ! environ.fpp: 248
                                                                                             ! environ.fpp: 249
      stop 'assertion failed'                                        ! punt                  ! environ.fpp: 250
                                                                                             ! environ.fpp: 251
   endif assertion_fails                                             ! expression false      ! environ.fpp: 252
                                                                                             ! environ.fpp: 253
return                                                               ! assert()              ! environ.fpp: 254
                                                                                             ! environ.fpp: 255
!  assert()                                                                                  ! environ.fpp: 256
                                                                                             ! environ.fpp: 257
end subroutine assert                                                                        ! environ.fpp: 258
                                                                                             ! environ.fpp: 259
! **********************************************************************                     ! environ.fpp: 260
                                                                                             ! environ.fpp: 261
!  public getopt: return next known option from command line or unknown                      ! environ.fpp: 262
                                                                                             ! environ.fpp: 263
integer function getopt( optstring)                                                          ! environ.fpp: 264
                                                                                             ! environ.fpp: 265
character( len= *), intent( in) :: optstring                                                 ! environ.fpp: 266
                                                                                             ! environ.fpp: 267
!  getopt() local                                                                            ! environ.fpp: 268
                                                                                             ! environ.fpp: 269
   character( len= optarg_len) :: optword                                                    ! environ.fpp: 270
   character( len= 1) :: firstchar, secndchar, thirdchar                                     ! environ.fpp: 271
                                                                                             ! environ.fpp: 272
!  character constants                                                                       ! environ.fpp: 273
                                                                                             ! environ.fpp: 274
   character( len= 1), parameter :: dash = '-'                                               ! environ.fpp: 275
   character( len= 1), parameter :: colon = ':'                                              ! environ.fpp: 276
   character( len= 1), parameter :: blank = ' '                                              ! environ.fpp: 277
                                                                                             ! environ.fpp: 278
!  integers                                                                                  ! environ.fpp: 279
                                                                                             ! environ.fpp: 280
   integer :: iopt                                                                           ! environ.fpp: 281
                                                                                             ! environ.fpp: 282
!  external symbols                                                                          ! environ.fpp: 283
                                                                                             ! environ.fpp: 284
   integer, external :: iargc                                                                ! environ.fpp: 285
                                                                                             ! environ.fpp: 286
   external :: getarg                                                                        ! environ.fpp: 287
                                                                                             ! environ.fpp: 288
! **********************************************************************                     ! environ.fpp: 289
                                                                                             ! environ.fpp: 290
!  getopt() text                                                                             ! environ.fpp: 291
                                                                                             ! environ.fpp: 292
continue                                                             ! getopt()              ! environ.fpp: 293
                                                                                             ! environ.fpp: 294
!  initialize for next option                                                                ! environ.fpp: 295
                                                                                             ! environ.fpp: 296
   optarg = blank                                                    ! reset                 ! environ.fpp: 297
                                                                                             ! environ.fpp: 298
   check_inc: if( optind >= iargc() )then                            ! if no unread options  ! environ.fpp: 299
                                                                                             ! environ.fpp: 300
      optarg = unknown_option                                        ! complain              ! environ.fpp: 301
      getopt = end_of_args                                           ! and                   ! environ.fpp: 302
                                                                                             ! environ.fpp: 303
      return                                                         ! quit                  ! environ.fpp: 304
                                                                                             ! environ.fpp: 305
   endif check_inc                                                   ! if no unread options  ! environ.fpp: 306
                                                                                             ! environ.fpp: 307
!  get next option                                                                           ! environ.fpp: 308
                                                                                             ! environ.fpp: 309
   optind = optind + 1                                               ! next arg to get       ! environ.fpp: 310
                                                                                             ! environ.fpp: 311
   call getarg( optind, optword)                                     ! get next arg word     ! environ.fpp: 312
                                                                                             ! environ.fpp: 313
   firstchar = optword( 1: 1)                                        ! examine optword       ! environ.fpp: 314
   secndchar = optword( 2: 2)                                        ! character by          ! environ.fpp: 315
   thirdchar = optword( 3: 3)                                        ! character             ! environ.fpp: 316
                                                                                             ! environ.fpp: 317
!  if word is not -x                                                                         ! environ.fpp: 318
                                                                                             ! environ.fpp: 319
   not_an_option: if( firstchar /= dash )then                        ! if word is not an option ! environ.fpp: 320
                                                                                                ! environ.fpp: 321
      optarg = optword                                               ! return word              ! environ.fpp: 322
      getopt = ichar( unknown_option)                                ! mark unrecognized        ! environ.fpp: 323
                                                                                                ! environ.fpp: 324
      return                                                         ! quit                     ! environ.fpp: 325
                                                                                                ! environ.fpp: 326
!  if word is --                                                                                ! environ.fpp: 327
                                                                                                ! environ.fpp: 328
   elseif( secndchar == dash )then not_an_option                     ! if --                    ! environ.fpp: 329
                                                                                                ! environ.fpp: 330
      optarg = optword                                               ! return --                ! environ.fpp: 331
      getopt = end_of_args                                           ! signal end of options    ! environ.fpp: 332
                                                                                                ! environ.fpp: 333
      return                                                         ! quit                     ! environ.fpp: 334
                                                                                                ! environ.fpp: 335
   endif not_an_option                                                                          ! environ.fpp: 336
                                                                                                ! environ.fpp: 337
!  optword is -x (not --)                                                                       ! environ.fpp: 338
                                                                                                ! environ.fpp: 339
   iopt = index( optstring, secndchar)                               ! find optletter in string ! environ.fpp: 340
                                                                                                ! environ.fpp: 341
   is_opt: if( iopt > substring_not_found )then                      ! if found in optstring    ! environ.fpp: 342
                                                                                                ! environ.fpp: 343
!  if this optltr must have another word                                                        ! environ.fpp: 344
                                                                                                ! environ.fpp: 345
      opt_string: if( optstring( iopt+1: iopt+1) == colon )then                                 ! environ.fpp: 346
                                                                                                ! environ.fpp: 347
!  it can be separated by a blank                                                               ! environ.fpp: 348
                                                                                                ! environ.fpp: 349
         next_word: if( thirdchar == blank )then                     ! in which case            ! environ.fpp: 350
                                                                                                ! environ.fpp: 351
            optind = optind + 1                                      ! increment index          ! environ.fpp: 352
            call getarg( optind, optarg)                             ! get next word            ! environ.fpp: 353
                                                                                                ! environ.fpp: 354
!  or not be separated by a blank                                                               ! environ.fpp: 355
                                                                                                ! environ.fpp: 356
         else next_word                                                                         ! environ.fpp: 357
                                                                                                ! environ.fpp: 358
            optarg = optword( 3: )                                   ! option field is rest of word ! environ.fpp: 359
                                                                                                    ! environ.fpp: 360
         endif next_word                                                                            ! environ.fpp: 361
                                                                                                    ! environ.fpp: 362
      endif opt_string                                                                              ! environ.fpp: 363
                                                                                                    ! environ.fpp: 364
      getopt = ichar( secndchar)                                     ! option found                 ! environ.fpp: 365
                                                                                                    ! environ.fpp: 366
!  if this optltr must not have another word                                                        ! environ.fpp: 367
                                                                                                    ! environ.fpp: 368
   else is_opt                                                       ! if not found in optstring    ! environ.fpp: 369
                                                                                                    ! environ.fpp: 370
      optarg = optword                                               ! return word and              ! environ.fpp: 371
      getopt = ichar( unknown_option)                                ! complain                     ! environ.fpp: 372
                                                                                                    ! environ.fpp: 373
   endif is_opt                                                                                     ! environ.fpp: 374
                                                                                                    ! environ.fpp: 375
return                                                               ! getopt()                     ! environ.fpp: 376
                                                                                                    ! environ.fpp: 377
!  getopt()                                                                                         ! environ.fpp: 378
                                                                                                    ! environ.fpp: 379
end function getopt                                                                                 ! environ.fpp: 380
                                                                                                    ! environ.fpp: 381
! **********************************************************************                            ! environ.fpp: 382
                                                                                                    ! environ.fpp: 383
!  lshift()/rshift()                                                                                ! environ.fpp: 384
                                                                                                    ! environ.fpp: 385
! **********************************************************************                            ! environ.fpp: 409
                                                                                                    ! environ.fpp: 409
!  ?kind_lshift(): lshift for kind byte                                                             ! environ.fpp: 409
                                                                                                    ! environ.fpp: 409
elemental integer function byte_lshift( i, j)                                                       ! environ.fpp: 409
                                                                                                    ! environ.fpp: 409
integer( kind= byte_k), intent( in) :: i, j                                                         ! environ.fpp: 409
                                                                                                    ! environ.fpp: 409
!  byte_lshift() text                                                                               ! environ.fpp: 409
                                                                                                    ! environ.fpp: 409
continue                                                             ! lshift()                     ! environ.fpp: 409
                                                                                                    ! environ.fpp: 409
   byte_lshift = ishft( i, abs( j))                                                                 ! environ.fpp: 409
                                                                                                    ! environ.fpp: 409
return                                                               ! lshift()                     ! environ.fpp: 409
                                                                                                    ! environ.fpp: 409
!  byte_lshift()                                                                                    ! environ.fpp: 409
                                                                                                    ! environ.fpp: 409
end function byte_lshift                                                                            ! environ.fpp: 409
                                                                                                    ! environ.fpp: 409
! **********************************************************************                            ! environ.fpp: 412
                                                                                                    ! environ.fpp: 412
!  ?kind_lshift(): lshift for kind short                                                            ! environ.fpp: 412
                                                                                                    ! environ.fpp: 412
elemental integer function short_lshift( i, j)                                                      ! environ.fpp: 412
                                                                                                    ! environ.fpp: 412
integer( kind= short_k), intent( in) :: i, j                                                        ! environ.fpp: 412
                                                                                                    ! environ.fpp: 412
!  short_lshift() text                                                                              ! environ.fpp: 412
                                                                                                    ! environ.fpp: 412
continue                                                             ! lshift()                     ! environ.fpp: 412
                                                                                                    ! environ.fpp: 412
   short_lshift = ishft( i, abs( j))                                                                ! environ.fpp: 412
                                                                                                    ! environ.fpp: 412
return                                                               ! lshift()                     ! environ.fpp: 412
                                                                                                    ! environ.fpp: 412
!  short_lshift()                                                                                   ! environ.fpp: 412
                                                                                                    ! environ.fpp: 412
end function short_lshift                                                                           ! environ.fpp: 412
                                                                                                    ! environ.fpp: 412
! **********************************************************************                            ! environ.fpp: 415
                                                                                                    ! environ.fpp: 415
!  ?kind_lshift(): lshift for kind int                                                              ! environ.fpp: 415
                                                                                                    ! environ.fpp: 415
elemental integer function int_lshift( i, j)                                                        ! environ.fpp: 415
                                                                                                    ! environ.fpp: 415
integer( kind= int_k), intent( in) :: i, j                                                          ! environ.fpp: 415
                                                                                                    ! environ.fpp: 415
!  int_lshift() text                                                                                ! environ.fpp: 415
                                                                                                    ! environ.fpp: 415
continue                                                             ! lshift()                     ! environ.fpp: 415
                                                                                                    ! environ.fpp: 415
   int_lshift = ishft( i, abs( j))                                                                  ! environ.fpp: 415
                                                                                                    ! environ.fpp: 415
return                                                               ! lshift()                     ! environ.fpp: 415
                                                                                                    ! environ.fpp: 415
!  int_lshift()                                                                                     ! environ.fpp: 415
                                                                                                    ! environ.fpp: 415
end function int_lshift                                                                             ! environ.fpp: 415
                                                                                                    ! environ.fpp: 415
! **********************************************************************                            ! environ.fpp: 418
                                                                                                    ! environ.fpp: 418
!  ?kind_lshift(): lshift for kind long                                                             ! environ.fpp: 418
                                                                                                    ! environ.fpp: 418
elemental integer function long_lshift( i, j)                                                       ! environ.fpp: 418
                                                                                                    ! environ.fpp: 418
integer( kind= long_k), intent( in) :: i, j                                                         ! environ.fpp: 418
                                                                                                    ! environ.fpp: 418
!  long_lshift() text                                                                               ! environ.fpp: 418
                                                                                                    ! environ.fpp: 418
continue                                                             ! lshift()                     ! environ.fpp: 418
                                                                                                    ! environ.fpp: 418
   long_lshift = ishft( i, abs( j))                                                                 ! environ.fpp: 418
                                                                                                    ! environ.fpp: 418
return                                                               ! lshift()                     ! environ.fpp: 418
                                                                                                    ! environ.fpp: 418
!  long_lshift()                                                                                    ! environ.fpp: 418
                                                                                                    ! environ.fpp: 418
end function long_lshift                                                                            ! environ.fpp: 418
                                                                                                    ! environ.fpp: 418
! **********************************************************************                            ! environ.fpp: 443
                                                                                                    ! environ.fpp: 443
!  byte_rshift(): rshift for kind byte                                                              ! environ.fpp: 443
                                                                                                    ! environ.fpp: 443
elemental integer function byte_rshift( i, j)                                                       ! environ.fpp: 443
                                                                                                    ! environ.fpp: 443
integer( kind= byte_k), intent( in) :: i, j                                                         ! environ.fpp: 443
                                                                                                    ! environ.fpp: 443
!  byte_rshift() text                                                                               ! environ.fpp: 443
                                                                                                    ! environ.fpp: 443
continue                                                             ! rshift()                     ! environ.fpp: 443
                                                                                                    ! environ.fpp: 443
   byte_rshift = ishft( i, -abs( j))                                                                ! environ.fpp: 443
                                                                                                    ! environ.fpp: 443
return                                                               ! rshift()                     ! environ.fpp: 443
                                                                                                    ! environ.fpp: 443
!  byte_rshift()                                                                                    ! environ.fpp: 443
                                                                                                    ! environ.fpp: 443
end function byte_rshift                                                                            ! environ.fpp: 443
                                                                                                    ! environ.fpp: 443
! **********************************************************************                            ! environ.fpp: 446
                                                                                                    ! environ.fpp: 446
!  short_rshift(): rshift for kind short                                                            ! environ.fpp: 446
                                                                                                    ! environ.fpp: 446
elemental integer function short_rshift( i, j)                                                      ! environ.fpp: 446
                                                                                                    ! environ.fpp: 446
integer( kind= short_k), intent( in) :: i, j                                                        ! environ.fpp: 446
                                                                                                    ! environ.fpp: 446
!  short_rshift() text                                                                              ! environ.fpp: 446
                                                                                                    ! environ.fpp: 446
continue                                                             ! rshift()                     ! environ.fpp: 446
                                                                                                    ! environ.fpp: 446
   short_rshift = ishft( i, -abs( j))                                                               ! environ.fpp: 446
                                                                                                    ! environ.fpp: 446
return                                                               ! rshift()                     ! environ.fpp: 446
                                                                                                    ! environ.fpp: 446
!  short_rshift()                                                                                   ! environ.fpp: 446
                                                                                                    ! environ.fpp: 446
end function short_rshift                                                                           ! environ.fpp: 446
                                                                                                    ! environ.fpp: 446
! **********************************************************************                            ! environ.fpp: 449
                                                                                                    ! environ.fpp: 449
!  int_rshift(): rshift for kind int                                                                ! environ.fpp: 449
                                                                                                    ! environ.fpp: 449
elemental integer function int_rshift( i, j)                                                        ! environ.fpp: 449
                                                                                                    ! environ.fpp: 449
integer( kind= int_k), intent( in) :: i, j                                                          ! environ.fpp: 449
                                                                                                    ! environ.fpp: 449
!  int_rshift() text                                                                                ! environ.fpp: 449
                                                                                                    ! environ.fpp: 449
continue                                                             ! rshift()                     ! environ.fpp: 449
                                                                                                    ! environ.fpp: 449
   int_rshift = ishft( i, -abs( j))                                                                 ! environ.fpp: 449
                                                                                                    ! environ.fpp: 449
return                                                               ! rshift()                     ! environ.fpp: 449
                                                                                                    ! environ.fpp: 449
!  int_rshift()                                                                                     ! environ.fpp: 449
                                                                                                    ! environ.fpp: 449
end function int_rshift                                                                             ! environ.fpp: 449
                                                                                                    ! environ.fpp: 449
! **********************************************************************                            ! environ.fpp: 452
                                                                                                    ! environ.fpp: 452
!  long_rshift(): rshift for kind long                                                              ! environ.fpp: 452
                                                                                                    ! environ.fpp: 452
elemental integer function long_rshift( i, j)                                                       ! environ.fpp: 452
                                                                                                    ! environ.fpp: 452
integer( kind= long_k), intent( in) :: i, j                                                         ! environ.fpp: 452
                                                                                                    ! environ.fpp: 452
!  long_rshift() text                                                                               ! environ.fpp: 452
                                                                                                    ! environ.fpp: 452
continue                                                             ! rshift()                     ! environ.fpp: 452
                                                                                                    ! environ.fpp: 452
   long_rshift = ishft( i, -abs( j))                                                                ! environ.fpp: 452
                                                                                                    ! environ.fpp: 452
return                                                               ! rshift()                     ! environ.fpp: 452
                                                                                                    ! environ.fpp: 452
!  long_rshift()                                                                                    ! environ.fpp: 452
                                                                                                    ! environ.fpp: 452
end function long_rshift                                                                            ! environ.fpp: 452
                                                                                                    ! environ.fpp: 452
! **********************************************************************                            ! environ.fpp: 454
                                                                                                    ! environ.fpp: 455
!  cut(): return input string broken at separators in output array                                  ! environ.fpp: 456
                                                                                                    ! environ.fpp: 457
pure subroutine cut( string, sep, substrings, substring_len, number)                                ! environ.fpp: 458
                                                                                                    ! environ.fpp: 459
character( len= *), intent( in) :: string                            ! input string                 ! environ.fpp: 460
character( len= 1), intent( in) :: sep                               ! separator                    ! environ.fpp: 461
                                                                                                    ! environ.fpp: 462
character( len= *), dimension( :), optional, intent( out) :: substrings                             ! environ.fpp: 463
integer, dimension( *), optional, intent( out) :: substring_len                                     ! environ.fpp: 464
                                                                                                    ! environ.fpp: 465
integer, optional, intent( out) :: number                                                           ! environ.fpp: 466
                                                                                                    ! environ.fpp: 467
!  cut() local                                                                                      ! environ.fpp: 468
                                                                                                    ! environ.fpp: 469
   integer :: index_sep                                              ! index of next separator      ! environ.fpp: 470
   integer :: next_substring                                         ! start of next substring      ! environ.fpp: 471
   integer :: number_substring                                       ! number of substring found    ! environ.fpp: 472
                                                                                                    ! environ.fpp: 473
!  cut() text                                                                                       ! environ.fpp: 474
                                                                                                    ! environ.fpp: 475
continue                                                             ! cut()                        ! environ.fpp: 476
                                                                                                    ! environ.fpp: 477
!  initialize                                                                                       ! environ.fpp: 478
                                                                                                    ! environ.fpp: 479
   next_substring = 1                                                ! start of next substring      ! environ.fpp: 480
   number_substring = 0                                              ! number found so far          ! environ.fpp: 481
                                                                                                    ! environ.fpp: 482
   index_sep = index( string( next_substring: ), sep)                ! index of first separator     ! environ.fpp: 483
                                                                                                    ! environ.fpp: 484
!  loop while more separators in string                                                             ! environ.fpp: 485
                                                                                                    ! environ.fpp: 486
   more_subs: do while( index_sep > substring_not_found)             ! while more separators        ! environ.fpp: 487
                                                                                                    ! environ.fpp: 488
      number_substring = number_substring + 1                        ! one more substring           ! environ.fpp: 489
                                                                                                    ! environ.fpp: 490
      set_substring: if( present( substrings) )then                  ! next substring               ! environ.fpp: 491
                                                                                                    ! environ.fpp: 492
         substrings( number_substring) = string( next_substring: next_substring + index_sep - 2)    ! environ.fpp: 493
                                                                                                    ! environ.fpp: 494
      endif set_substring                                            ! next substring               ! environ.fpp: 495
                                                                                                    ! environ.fpp: 496
      set_len: if( present( substring_len) )then                     ! next length                  ! environ.fpp: 497
                                                                                                    ! environ.fpp: 498
         substring_len( number_substring) = index_sep - 1                                           ! environ.fpp: 499
                                                                                                    ! environ.fpp: 500
      endif set_len                                                  ! next length                  ! environ.fpp: 501
                                                                                                    ! environ.fpp: 502
      next_substring = next_substring + index_sep                    ! start of next substring      ! environ.fpp: 503
                                                                                                    ! environ.fpp: 504
      index_sep = index( string( next_substring: ), sep)             ! index of next separator      ! environ.fpp: 505
                                                                                                    ! environ.fpp: 506
   enddo more_subs                                                   ! while more separators        ! environ.fpp: 507
                                                                                                    ! environ.fpp: 508
!  last substring                                                                                   ! environ.fpp: 509
                                                                                                    ! environ.fpp: 510
   number_substring = number_substring + 1                           ! one more substring           ! environ.fpp: 511
                                                                                                    ! environ.fpp: 512
   last_substring: if( present( substrings) )then                    ! rest of string is last substring ! environ.fpp: 513
                                                                                                        ! environ.fpp: 514
      substrings( number_substring) = string( next_substring: )                                         ! environ.fpp: 515
                                                                                                        ! environ.fpp: 516
   endif last_substring                                              ! rest of string is last substring ! environ.fpp: 517
                                                                                                        ! environ.fpp: 518
   len_arg: if( present( substring_len) )then                                                           ! environ.fpp: 519
                                                                                                        ! environ.fpp: 520
       substring_len( number_substring) = len_trim( string( next_substring: ))                          ! environ.fpp: 521
                                                                                                        ! environ.fpp: 522
    endif len_arg                                                                                       ! environ.fpp: 523
                                                                                                        ! environ.fpp: 524
!  return number of substring of requested                                                              ! environ.fpp: 525
                                                                                                        ! environ.fpp: 526
   number_arg: if( present( number) )then                                                               ! environ.fpp: 527
                                                                                                        ! environ.fpp: 528
      number = number_substring                                      ! count if requested               ! environ.fpp: 529
                                                                                                        ! environ.fpp: 530
   endif number_arg                                                                                     ! environ.fpp: 531
                                                                                                        ! environ.fpp: 532
return                                                               ! cut()                            ! environ.fpp: 533
                                                                                                        ! environ.fpp: 534
!  cut()                                                                                                ! environ.fpp: 535
                                                                                                        ! environ.fpp: 536
end subroutine cut                                                                                      ! environ.fpp: 537
                                                                                                        ! environ.fpp: 538
! **********************************************************************                                ! environ.fpp: 539
                                                                                                        ! environ.fpp: 540
!  swab(): return input string broken at separators in output array                                     ! environ.fpp: 541
                                                                                                        ! environ.fpp: 542
pure subroutine swab( a, b)                                                                             ! environ.fpp: 543
                                                                                                        ! environ.fpp: 544
integer( kind= int_k), dimension( :), intent( in) :: a                                                  ! environ.fpp: 545
                                                                                                        ! environ.fpp: 546
integer( kind= int_k), dimension( :), intent( out) :: b                                                 ! environ.fpp: 547
                                                                                                        ! environ.fpp: 548
!  swab() local                                                                                         ! environ.fpp: 549
                                                                                                        ! environ.fpp: 550
   integer( kind= byte_k), dimension( csu_per_nsu) :: temp                                              ! environ.fpp: 551
                                                                                                        ! environ.fpp: 552
   integer( kind= int_k) :: iword                                                                       ! environ.fpp: 553
                                                                                                        ! environ.fpp: 554
!  swab() text                                                                                          ! environ.fpp: 555
                                                                                                        ! environ.fpp: 556
continue                                                             ! swab()                           ! environ.fpp: 557
                                                                                                        ! environ.fpp: 558
   copy: do iword = 1, min( size(a), size( b) )                      ! copy words                       ! environ.fpp: 559
                                                                                                        ! environ.fpp: 560
      temp = transfer( a( iword), temp)                              ! to byte array                    ! environ.fpp: 561
                                                                                                        ! environ.fpp: 562
      temp( 1: 2) = temp( 2: 1: -1)                                  ! swap low bytes                   ! environ.fpp: 563
      temp( 3: 4) = temp( 4: 3: -1)                                  ! swap hi bytes                    ! environ.fpp: 564
                                                                                                        ! environ.fpp: 565
      b( iword) = transfer( temp, b( iword) )                        ! from byte array                  ! environ.fpp: 566
                                                                                                        ! environ.fpp: 567
   enddo copy                                                        ! copy words                       ! environ.fpp: 568
                                                                                                        ! environ.fpp: 569
return                                                               ! swab()                           ! environ.fpp: 570
                                                                                                        ! environ.fpp: 571
!  swab()                                                                                               ! environ.fpp: 572
                                                                                                        ! environ.fpp: 573
end subroutine swab                                                                                     ! environ.fpp: 574
                                                                                                        ! environ.fpp: 575
! **********************************************************************                                ! environ.fpp: 576
                                                                                                        ! environ.fpp: 577
!  environment                                                                                          ! environ.fpp: 578
                                                                                                        ! environ.fpp: 579
! $Id: environ.fpp 1.4 2003/10/03 19:35:55Z Dan Release $                                               ! environ.fpp: 580
! **********************************************************************                                ! environ.fpp: 581
                                                                                                        ! environ.fpp: 582
end module environment                                               ! eof                              ! environ.fpp: 583
