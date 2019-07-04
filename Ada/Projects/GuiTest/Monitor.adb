-- *********************************************************************************************************************
-- *                               (c) 2016 by Soudronic AG, Bergdietikon, Switzerland                                 *
-- *                      Developed by White Elephant GmbH, Switzerland (www.white-elephant.ch)                        *
-- *********************************************************************************************************************
pragma Style_Soudronic;
with Gui;
with Gui.Aicwl;


package body Monitor is

   Monitor_Page      : Gui.Page;
   Timeline          : Gui.Aicwl.History;
   The_Channel       : Gui.Aicwl.History_Channel;
   Threshold_Channel : Gui.Aicwl.Line_Channel with Unreferenced;
   Freeze_Button     : Gui.Button;

   Is_Frozen : Boolean := False;

   task Worker is
     entry Start;
     entry Stop;
   end Worker;

   task body Worker is
     The_Value     : Gui.Aicwl.Data := 0.0;
     Is_Increasing : Boolean := True;
     use type Gui.Aicwl.Data;
   begin
     accept Start;
     loop
       select
         accept Stop;
         exit;
       or
         delay 1.0;
         Gui.Aicwl.Feed (The_Channel, The_Value);
         if Is_Increasing then
           if The_Value >= 9.9 then
             The_Value := 9.0;
             Is_Increasing := False;
           else
             The_Value := The_Value + 1.0;
           end if;
         else
           if The_Value <= -9.9 then
             The_Value := -9.0;
             Is_Increasing := True;
           else
             The_Value := The_Value - 1.0;
           end if;
         end if;
       end select;
     end loop;
   end Worker;


   procedure Freeze_Thaw is
   begin
     Gui.Disable (Freeze_Button);
     Is_Frozen := not Is_Frozen;
     if Is_Frozen then
       Gui.Set_Text (Freeze_Button, "Thaw");
       Gui.Aicwl.Freeze (Timeline);
     else
       Gui.Set_Text (Freeze_Button, "Freeze");
       Gui.Aicwl.Thaw (Timeline);
     end if;
     Gui.Enable (Freeze_Button);
   end Freeze_Thaw;


   procedure Start is
     use type Gui.Aicwl.Data;
   begin
     Monitor_Page := Gui.Add_Page ("Monitor");
     Timeline := Gui.Aicwl.Create (Monitor_Page, Page_Span => 40.0, User_Freeze => True);
     Gui.Aicwl.Enable_Zooming (Timeline, True);
     Gui.Aicwl.Define_Y_Axis (The_Graph => Timeline,
                              Y_Axis    => Gui.Aicwl.Left,
                              The_Width => 80,
                              With_Grid => False);
     Gui.Aicwl.Set_Y_Scale (The_Graph     => Timeline,
                            Y_Axis        => Gui.Aicwl.Left,
                            Lower_Y_Scale => -10.0,
                            Upper_Y_Scale => 10.0);

     The_Channel := Gui.Aicwl.Add (Timeline, Gui.Blue, 1000, Gui.Aicwl.Left);
     Threshold_Channel := Gui.Aicwl.Add (The_Graph     => Timeline,
                                         The_Color     => Gui.Red,
                                         Y_Axis        => Gui.Aicwl.Left,
                                         Initial_Value => 0.0,
                                         Annotation    => "Zero");
     Freeze_Button := Gui.Create (Parent_Page        => Monitor_Page,
                                  The_Text           => "Freeze",
                                  The_Action_Routine => Freeze_Thaw'access);
     Worker.Start;
   end Start;

   procedure Stop is
   begin
     Worker.Stop;
   end Stop;

end Monitor;
