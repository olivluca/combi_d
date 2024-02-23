# Combi D lin protocol

## Disclaimer

I don't have a CP Plus, so I obtained this data from my own experimentations based on the TrumaLogs found in [the WomoLin repository](https://github.com/muccc/WomoLIN), the spreadsheet available in the [Truma Inetbox Reverse Engineering repository](https://gitlab.womolin.de/public-repository/truma-inet-box-reverse-engineering) and the invaluable help from the kind people on the [Womo_Lin telegram channel](https://t.me/Womo_Lin).

The information herein may be inaccurate so be carefult if you try to hack your Combi, it's an expensive device and I don't assume any responsibility for any damage it may cause.

## Missing things

I still don't know how to turn on the boiler (only the heating), how to decode the error codes in the diagnostic reply (one of the bytes is surely part of the code, but there are codes over 255 so there must be some bits in another byte, also I don't know how to distinguish between an error and a warning) and how to reset an error. Also I couldn't find a way to activate the "room boost" mode.

## Control sequence

To control the Combi D from the lin bus (i.e. to emulate a cp-plus) you have to repeatedly send and read lin frames with the following IDs

All the frames are eight bytes long.

|  Frame ID | Read/Write |  Purpose |
|--|--|--|
|0x03| Write | Temperature setpoint |
|0x04| Write | Water temperature setpoint |
|0x05| Write | Energy selection|
|0x06| Write | Power limit |
|0x07| Write | Fan and room boost |
|0x16| Read | Status |
|0x3C| Write | Diagnostic frame |
|0x3D| Read | Diagnostic reply |

If you stop sending frames, the Combi will stop after about one minute.

## Temperature encoding
The temperature in the setpoint and in the status message is in degrees Kelvin multiplied by 10, it is encoded in two bytes, LSB and MSB.

To obtain the setpoint from ºC you have to add 273 and multiply by 10, e.g. 25.4ºC &rarr; (25.4-273)*10=2984, which in hex is 0x0BA8 so the two bytes will be 0xA8, 0x0B.

## Voltage encoding
In the status frame two bytes, LSB and MSB, encode the supply voltage. To obtain the voltage you have to multiply the value by 0.01 and subtract 327.67.

If the two bytes are 0x31, 0x85 the voltage will be 0x8531*0.01-327.67 &rarr; 13.3V

## Frame 0x03, Temperature setpoint

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|Setpoint low|Setpoint high|0xff|0xff|0xff|0xff|0xff|0xff|

To turn the heating on you should give here a positive setpoint. With 0.0 (0xAA, 0X0A) the heating will turn off.
For this to work the fan (frame 0x07) should be 0x00 (off), 0x01 (eco) or 0x02 (high).
I don't know if the Combi interprets corretly setpoints with decimals, the cp plus only allows to set the temperature in 1º increments between 5º and 30º. 


## Frame 0x04, Water temperature setpoint

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|Setpoint low|Setpoint high|0xff|0xff|0xff|0xff|0xff|0xff|

The possible setpoints should 0 (boiler off), 40 (boiler eco), 55 (boiler hot) and 60 (boost).

I don't know if other setpoints are accepted or if you have to limit the setpoints if the heating is also on.

**It seems this frame is doing nothing**

## Frame 0x05, Energy selection

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|Energy selection|0xff|0xff|0xff|0xff|0xff|0xff|0xff|

Here the first bytes encode the energy selection

|Value|Energy selection|
|--|--|
|0x00|None|
|0x01|Diesel|
|0x02|Electricity|
|0x03|Mix|

I dont' have a Combi E so I always use 0x01 here

## Frame 0x06, Power limit

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|LSB|MSB|0xff|0xff|0xff|0xff|0xff|0xff|0xff|

The possible values are 0x0000 for diesel, 0x0384 (900 in decimal) for 900W electricity and 0x0708 (1800 in decimal) for 1800W electricity. Remember to swap the bytes when sending the frame.

Again, since I don't have a Combi E I alwayse use 0x00, 0x00.

## Frame 0x07, fan and room boost (?)

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|Fan|Boost|0xff|0xff|0xff|0xff|0xff|0xff|

The first byte is the fan mode:

|Value|Mode|
|--|--|
|0x00|off **actually it turns on the boiler**|
|0x01|normal curve (eco?)|
|0x02|strong curve (high?)|
|0x10|speed 0 (off)|
|0x11|speed 1|
|0x12|speed 2|
|0x13|speed 3|
|0x14|speed 4|
|0x15|speed 5|
|0x16|speed 6|
|0x17|speed 7|
|0x18|speed 8|
|0x19|speed 9|
|0x1A|speed 10|

The manual mode overrides the heating, for the heating to work you have to set this to 0x00, 0x01, 0x02.

*Note: setting this to 0x00 with 0º setpoint for heating will turn on the boiler (?)*.

The second byte is supposedly the room boost (?), 0x00 if disabled or 0x01 if enabled. I'm not really sure about it. It doesn't make a difference.

## Frame 0x16, Status information

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|Extended status|Status bits|Room temperature LSB|Room temperature MSB|Water temperature LSB|Water temperature MSB|Supply voltage LSB|Supply voltage MSB|

For the temperature and voltage values see the respective sections. The values are updated only if the cp plus indicates it is active (i.e. when it sends a 0x01 in byte D3 of the diagnostic request 1, see below).

### Extended status

Extended status should be one of the values in this table, but I always see it at 0x00

|Value|Status|
|--|--|
|0x00|Everything off|
|0x04|BO active|
|0x08|RB active|
|0x10|RB available|
|0x14|RB available, BO active|
|0x18|RB available, active|
|0x1C|RB available, active, BO active|

### Status bits

This is the meaning of the status bits:

|Bit|Meaning|
|--|--|
|0|easi status (?)|
|1|220V supply status|
|2|window status (1=closed)|
|3|room demand|
|4|water demand|
|5||
|6||
|7|error|

## Frames 0x3c,0x3d diagnostic request/diagnostic reply

The request and reply frames follow the lin specifications (for single frame PDUs):

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|Node address|Frame type|Function/Function reply|D1|D2|D3|D4|D5|

The node address is always 0x01, the frame type is always single frame (left nibble 0, right nibble specifies the length of the payload).
 
There are two different requests, each one sent on alternate cycles.

### request 1

|Frame type|Function|D1|D2|D3|D4|D5|
|--|--|--|--|--|--|--|
|0x04|0xB8|0x10|0x03|0x00 or 0x01|0xFF|0xFF|

In D3 you should send a 0x01 to indicate the CP-Plus is on, 0x00 if it's off. The Combi will only accept commands if D3 is 0x01 (the fan is an exception, it will also work if D1 in the reply is 0x03). The values in frame 0x16 are not updated if it's 0x00. Since it doesn't seem it draws more current either way, I guess it's no problem leaving it to 0x01 always. 

### reply 1
|Frame type|Function reply|D1|D2|D3|D4|D5|
|--|--|--|--|--|--|--|
|0x03|0xF8|??|??|0xFF0|0xFF|0xFF|

D1 and D2 have the same value (though D2 might lag behind D1), the values I saw are 0x01, 0x02 and 0x03.

I think 0x01 means "ready", 0x02 "on" (acknowledging D3 in the request) and 0x03 "working" (heater or boiler on).

In ventilation mode it never shows 0x03 even if the fan is running, unless you send a 0x01 in D3 in the request. In that case it shows 0x03 briefly.

### request 2
|Frame type|Function|D1|D2|D3|D4|D5|
|--|--|--|--|--|--|--|
|0x06|0xB2|0x23|0x16|0x46|0x10|0x03|

Meaning unknown

### reply 2
|Frame type|Function reply|D1|D2|D3|D4|D5|
|--|--|--|--|--|--|--|
|0x06|0xF2|??|??|??|??|??|

I always see D1 at 0x01 and D5 at 0xFF. I don't know the meaning of D2, D3 and D4.
Most probably D3 is the error code but there must be some bits of the error code in one of the other bytes, since there are codes above 255.

I could only force two errors, both below 255

|Error|D2-D3-D4|Notes|
|--|--|--|
|Window open|0x02, 0xA2, 0x03 or 0x02, 0xA2, 0x00| 0xA2 is 162, which is the code for the window open according to the cp plus manual. This error resets automatically when the window is closed|
|Temperature sensor|0x14,0x6F,0x05|0x6F is 111, "temperature sensor". To reset this code I had to remove power to the Combi| 
