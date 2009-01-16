// Sent by Michael Dayah

foo = 0xff;
bar = 0xFF;
result = start + ((( Math.round(((((end & 0xFF0000) >> 16) - ((start    
& 0xFF0000) >> 16)) * n))) << 16) + (( Math.round(((((end & 0x00FF00) >> 8)     
- ((start & 0x00FF00) >> 8)) * n))) << 8) + (( Math.round((((end & 0x0000FF)    
- (start & 0x0000FF)) * n)))));
