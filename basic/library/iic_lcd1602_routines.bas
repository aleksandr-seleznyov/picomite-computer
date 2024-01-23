' -----------------------------------------------------------
' Routines for Micromite to drive serial I2C LCD
' by Andy H. 17/05/2017
' Adapted from a program by Jim Rowe.
' Futhure adapted from this post: https://www.thebackshed.com/forum/ViewTopic.php?TID=9642&PID=106063#106063#106063
' 3 calls in total:
' initLCD - Initialises the LCD Controller
' ClearScreen - Clears the display and sets cursor to line 1 position 1
' DisplayText row, offset, text$ - which row to output on, repositions cursor to offset, txt to be displayed



' subroutine to Display text on LCD.
sub DisplayText row, offset, text$
local i
select case row
case 1
sendcmdbyte (line1home + offset)
case 2
sendcmdbyte line2home + offset)
case 3
sendcmdbyte (line1home + 20 + offset)
case 4
sendcmdbyte (line2home + 20 + offset)
end select
For i = 1 To Len(text$)
SendDataByte ASC(Mid$(text$, i, 1))
Next i
end sub
' -----------------------------------------------------------------

' subroutine to clear LCD screen
sub ClearScreen
SendCmdByte cnt(4)
PAUSE 400 ' pause for 400ms to give it time...
end sub

' -----------------------------------------------------------------

' subroutine to initialise the LCD
SUB InitLCD
' declare the PCF8574 I2C address (all links out)
DIM AS LCDI2CAddr = &H27 ' (A2=A1=A0=1)

' masks for preparing I2C bytes for writing to 8574 & LCD
DIM RSCmask = &B00000000 ' select cmd register (RS = 0)
DIM RSDmask = &B00000001 ' or data register (RS = 1)
DIM ENmask = &B00000100 ' Enable (active = P2 high)
DIM BLmask = &B00001000 ' Turn on backlighting (P3 = 1)
DIM CNT(7) ' array to store LCD config command bytes
CNT(0) = &B00110011 ' &H33 --> 8-bit / 8-bit mode init
CNT(1) = &B00110010 ' &H32 --> 8-bit / 4-bit mode
CNT(2) = &B00101000 ' &H28 --> 4-bit mode, 2 lines, 5x7 pixels
CNT(3) = &B00001000 ' &H08 --> disp off, cursor off, no blink
CNT(4) = &B00000001 ' &H01 --> clear screen, return home cmd
CNT(5) = &B00000110 ' &H06 --> increment cursor, cursor move
CNT(6) = &B00001100 ' &H0C --> disp on, cursor off, no blink
DIM line1home = &B10000000 ' &H80 --> Place cursor at start of line 1
DIM line2home = &B11000000 ' &HC0 Place cursor at start of line 2

local i
For i = 0 To 6
SendCmdByte cnt(i)
IF i = 4 THEN
PAUSE 20 ' pause for 20ms to allow full clear
END IF
PAUSE 50 ' pause to let LCD controller settle down
Next
END SUB
' -----------------------------------------------------------------

' subroutine to send a command byte (aByte) to the LCD controller
SUB SendCmdByte aByte
local temp, RSbit
RSbit = RSCmask ' make sure we're sending to cmd reg (RS=0)
' then prepare the more significant nibble
temp = (aByte AND &HF0) Or RSbit OR BLmask
DirectSend temp ' and send it
' then prepare the less significant nibble
temp = ((aByte AND &H0F) << 4) Or RSbit OR BLmask
DirectSend temp ' and send it also
END SUB

' -----------------------------------------------------------------

' subroutine to send a data byte (aByte) to the LCD controller
SUB SendDataByte aByte
local temp, RSbit
RSbit = RSDmask ' make sure we're sending to data reg (RS=1)
' then prepare the more significant nibble (from aByte)
temp = (aByte AND &B01110000) Or RSbit OR BLmask
DirectSend temp ' and send it
' then prepare the less significant nibble (from aByte)
temp = ((aByte AND &H0F) << 4) OR RSbit OR BLmask
DirectSend temp ' and send it also
END SUB

' -----------------------------------------------------------------

' subroutine to send a nibble (in temp) to the LCD
SUB DirectSend temp
temp = temp Xor ENmask ' make EN=1
I2C WRITE LCDI2CAddr, 0, 1, temp ' send it with EN=1
Pause 10 ' pause for 10ms
temp = temp Xor ENmask ' now make EN=0 again
I2C WRITE LCDI2CAddr, 0, 1, temp ' before sending again
END SUB
' ----------------------------------------------------------------