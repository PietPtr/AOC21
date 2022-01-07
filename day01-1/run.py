import serial, time

ser = serial.Serial('/dev/ttyUSB0', 115200, timeout=0.5)
print(ser.name) 

with open("input.txt", "rb") as file:
    while True:
        char = file.read(1)         
        if not char:
            break
        ser.write(char)

print(ser.read(32))
# ser.close()