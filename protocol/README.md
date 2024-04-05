# Combi D lin protocol

## Disclaimer

I don't have a CP Plus, so I obtained this data from my own experimentations based on the TrumaLogs found in [the WomoLin repository](https://github.com/muccc/WomoLIN), the spreadsheet available in the [Truma Inetbox Reverse Engineering repository](https://gitlab.womolin.de/public-repository/truma-inet-box-reverse-engineering) and the invaluable help from the kind people on the [Womo_Lin telegram channel](https://t.me/Womo_Lin).

The information herein may be inaccurate so be careful if you try to hack your Combi, it's an expensive device and I don't assume any responsibility for any damage it may cause.

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

There are some extra diagnostic frames that are not normally available but
can be enabled if needed.

## Temperature encoding
The temperature in the setpoint and in the status message is in degrees Kelvin multiplied by 10, it is encoded in two bytes, LSB and MSB.

To obtain the setpoint from ºC you have to add 273 and multiply by 10, e.g. 25.4ºC &rarr; (25.4+273)*10=2984, which in hex is 0x0BA8 so the two bytes will be 0xA8, 0x0B.

## Voltage encoding
In the status frame two bytes, LSB and MSB, encode the supply voltage. To obtain the voltage you have to multiply the value by 0.01 and subtract 327.67 (or subtract 32767 and multiply by 0.01).

If the two bytes are 0x31, 0x85 the voltage will be 0x8531*0.01-327.67 &rarr; 13.3V

## Frame 0x03, Temperature setpoint

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|Setpoint low|Setpoint high|0xff|0xff|0xff|0xff|0xff|0xff|

To turn the heating on you should give here a positive setpoint. With 0.0 (0xAA, 0x0A) the heating will turn off.
For this to work the fan (frame 0x07) should be 0x00 (off), 0x01 (eco) or 0x02 (high).
I don't know if the Combi interprets correctly setpoints with decimals, the cp plus only allows to set the temperature in 1º increments between 5º and 30º. 


## Frame 0x04, Water temperature setpoint

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|Setpoint low|Setpoint high|0xff|0xff|0xff|0xff|0xff|0xff|

The possible setpoints are 0 (boiler off), 40 (boiler eco), 55 (boiler hot) and 60 (boost).

The boost is not managed by the heater but by the cp-plus: it should be kept
active at most for 40 minutes or when the water request goes from true to false
(meaning it has reached the setpoint). Also, if the water temperature is
less than 50º, it should keep the heating off (by giving a 0ºC setpoint in
frame 0x03).


## Frame 0x05, Energy selection

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|Energy selection|0xff|0xff|0xff|0xff|0xff|0xff|0xff|

Here the first byte encodes the energy selection

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

The possible values are 0x0000 for diesel, 0x0384 (900 in decimal) for 900W electricity and 0x0708 (1800 in decimal) for 1800W electricity. Remember to swap the bytes when sending the frame (0x84, 0x03 for 900W and 0x08, 0x07 for 1800W).

Again, since I don't have a Combi E I alwayse use 0x00, 0x00.

## Frame 0x07, fan

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|Fan|0xfe|0xff|0xff|0xff|0xff|0xff|0xff|

The first byte is the fan mode:

|Value|Mode|
|--|--|
|0x00|off **actually it turns on the boiler without turning on the heating**|
|0x01|eco|
|0x02|high|
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

The value should be ORed with 0xe0, e.g. eco should be 0xe1.

The manual mode overrides the heating, for the heating to work you have to set
the fan  to 0x00, 0x01, 0x02.

*Note: setting this to 0x00 with 0,40 or 55 setpoint for water will turn on the boiler (?)
so I recommend to use manual speed 0 (0x10) instead*.

The second byte is always 0xfe.

## Frame 0x16, Status information

|byte 0|byte 1|byte 2| byte 3|byte 4|byte 5|byte 6|byte 7|
|--|--|--|--|--|--|--|--|
|0x00|Status bits|Room temperature LSB|Room temperature MSB|Water temperature LSB|Water temperature MSB|Supply voltage LSB|Supply voltage MSB|

For the temperature and voltage values see the respective sections. The values are updated only if the
combi indicates it is active (i.e. when it sends a 0x01 in byte D3 of the diagnostic
reply 1, see below).

### Status bits

This is the meaning of the status bits:

|Bit|Meaning|
|--|--|
|0|antifreeze status|
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

### request 1 (on/off)

|Frame type|Function|D1|D2|D3|D4|D5|
|--|--|--|--|--|--|--|
|0x04|0xB8|0x10|0x03|0x00 or 0x01|0xFF|0xFF|

In D3 you should send a 0x01 to turn on the heater and 0x00 to turn it off. The Combi will only accept commands if D3 is 0x01 (the fan is an exception, it will also work if D1 in the reply is 0x03). The values in frame 0x16 are not updated if it's 0x00. Since it doesn't seem it draws more current either way, I guess it's no problem leaving it to 0x01 always.
In my implementation of the cp-plus emulator I set it to 1 when there is
something active (boiler, heating or fan) and set it to 0 after 10 seconds
with nothing active (so I can be sure the combi has received the fan speed
0 and won't turn on the boiler instead). 

### reply 1
|Frame type|Function reply|D1|D2|D3|D4|D5|
|--|--|--|--|--|--|--|
|0x03|0xF8|requested state|current state|0xFF|0xFF|0xFF|

D1 is the requested state and D2 is the current state, the values I saw are 0x01, 0x02 and 0x03.

I think 0x01 means "ready", 0x02 "on" (acknowledging D3 in the request) and 0x03
"shutdown".


### request 2 (get error code)
|Frame type|Function|D1|D2|D3|D4|D5|
|--|--|--|--|--|--|--|
|0x06|0xB2|0x23|0x16|0x46|0x10|0x03|

Meaning unknown

### reply 2
|Frame type|Function reply|D1|D2|D3|D4|D5|
|--|--|--|--|--|--|--|
|0x06|0xF2|0x01|error class|error code|error short|0xff|

Error class can be 0 (no error) 1,2 (warning) 10,20,30 (error) or 40 (heater
locked).

Error code is the numeric code of the error and Error short is the number of
times the error led will blink.


## Error reset

A warning (error class 1 or 2) will disappear by itself but an error (class
10,20 or 30) needs a reset sequence: first you turn off the heater
(using the first diagnostic request frame), wait fot the current state to
be 1 then stop the communication on the bus for 10 seconds.
