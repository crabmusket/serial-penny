void setup() {
  pinMode(RED_LED, OUTPUT);
  analogReference(INTERNAL1V5);
  analogRead(TEMPSENSOR);
  Serial.begin(9600);
}

void loop() {
  P1OUT ^= BIT6;
  int a = ((uint32_t)analogRead(TEMPSENSOR) * 27069 - 18169625) * 10 >> 16;
  Serial.println(a);
  delay(1000);
}

