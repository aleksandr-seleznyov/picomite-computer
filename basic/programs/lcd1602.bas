OPTION EXPLICIT
OPTION DEFAULT INTEGER

PAUSE 200 ' wait 200ms for LCD to stabilise after powerup
SETPIN GP0, GP1, I2C
I2C OPEN 100,100 
InitLCD 
ClearScreen
DisplayText 1, 0, MM.DEVICE$
DisplayText 2, 0, "Ver:" + FORMAT$(MM.VER)