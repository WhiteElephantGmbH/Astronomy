-- *********************************************************************************************************************
-- *                           (c) 2022 by White Elephant GmbH, Schaffhausen, Switzerland                              *
-- *                                               www.white-elephant.ch                                               *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Strings;

package body Response is

  function Item (With_Switches : ENC.Switches;
                 Components    : String) return String is

    function S (The_Port : ENC.Port) return String is
    begin
      return """state"":" & Strings.Trimmed(ENC.Switch'pos(With_Switches(The_Port))'image);
    end S;

  begin
    return
      "{" &
      (if Components in "512" | "513" then
      """hardware"":" &
        "{" &
        """num_powerports"":4," &
        """num_inputs"":8," &
        """num_idt"":0," &
        """num_slave_idt"":0," &
        """has"":" &
          "{" &
          """amp_meter"":0," &
          """amp_diff"":0," &
          """amp_neutral"":0," &
          """messages"":0," &
          """beeper"":0," &
          """snmp"":1," &
          """gsm"":0," &
          """sntp"":0," &
          """email"":1," &
          """twin"":0," &
          """ets"":0," &
          """slave"":0," &
          """serialconsole"":1," &
          """ext12vout"":0," &
          """ext3vout"":0," &
          """brightness"":0," &
          """buttonlock"":1," &
          """display"":0" &
          "}," &
        """provider_known"":0," &
        """outtype"":2," &
        """poe"":0," &
        """stopinput"":0," &
        """banks"":"&
          "[{" &
          """size"":4," &
          """name"":""""," &
          """ports"":[1,2,3,4]," &
          """power"":1," &
          """ZX counted"":0," &
          """ignore_powerloss"":0," &
          """fuse"":null" &
          "}]," &
        """power"":[{""inputs"":[],""outputs"":[]}]," &
        """powersupply"":{""inputs"":[],""extout"":[]}," &
        """eeprom"":{""size"":32768," &
                    """page_size"":64," &
                    """used"":7160," &
                    """max free block"":25608," &
                    """errors"":{""read"":0,""write"":0}" &
                    "}," &
        """flash"":{""id"":522715137,""page_size"":256,""page_num"":32768}" &
        "},"
      else
        ""
      ) &
      (if Components in "1" | "513" then
      """outputs"":" &
        "[" &
        "{""name"":""PC"","             & S(ENC.Port_1) & ",""type"":2,""batch"":[0,0,0,0,0,0],""wdog"":[0,3,null]}," &
        "{""name"":""CDK700 ON\/OFF""," & S(ENC.Port_2) & ",""type"":2,""batch"":[0,0,0,0,0,0],""wdog"":[0,3,null]}," &
        "{""name"":""-"","              & S(ENC.Port_3) & ",""type"":2,""batch"":[0,0,0,0,0,0],""wdog"":[0,3,null]}," &
        "{""name"":""12V\/USB_HUB"","   & S(ENC.Port_4) & ",""type"":2,""batch"":[0,0,0,0,0,0],""wdog"":[0,3,null]}" &
        "],"
      else
        ""
      ) &
      """eof"":1" &
      "}";
  end Item;

end Response;
