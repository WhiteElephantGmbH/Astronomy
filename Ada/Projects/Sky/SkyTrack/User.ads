-- *********************************************************************************************************************
-- *                       (c) 2011 .. 2021 by White Elephant GmbH, Schaffhausen, Switzerland                          *
-- *                                               www.white-elephant.ch                                               *
-- *                                                                                                                   *
-- *    This program is free software; you can redistribute it and/or modify it under the terms of the GNU General     *
-- *    Public License as published by the Free Software Foundation; either version 2 of the License, or               *
-- *    (at your option) any later version.                                                                            *
-- *                                                                                                                   *
-- *    This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the     *
-- *    implied warranty of MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE. See the GNU General Public License    *
-- *    for more details.                                                                                              *
-- *                                                                                                                   *
-- *    You should have received a copy of the GNU General Public License along with this program; if not, write to    *
-- *    the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.                *
-- *********************************************************************************************************************
pragma Style_White_Elephant;

with Error;
with Name;
with Telescope;

package User is

  type Action is (Define_Catalog, Define_Target, Park, Stop, Align, Synch, Go_To, Set_Orientation, Update, Close);

  subtype Button_Action is Action range Park .. Go_To;

  type Action_Handler is access procedure (The_Action : Action);

  type Percent is new Natural range 0 .. 100;

  type Selection is (All_Objects, Solar_System, Clusters, Open_Clusters, Nebulas, Galaxies,
                     Stars, Multiple_Stars, Near_Earth_Objects);

  subtype Object is Selection range Selection'succ(Selection'first) .. Selection'last;

  procedure Show_Error (The_Text : String := Error.Message);

  procedure Show (The_Progress : Percent);

  procedure Show (Visible_In : Duration);

  procedure Show (Information : Telescope.Data);

  procedure Clear_Target;

  procedure Execute (The_Startup_Handler     : not null access procedure;
                     The_Action_Handler      : Action_Handler;
                     The_Termination_Handler : not null access procedure);

  procedure Enter_Handling;

  procedure Perform_Synch;

  procedure Perform_Goto;

  procedure Perform_Stop;

  procedure Clear_Targets;

  procedure Define (Targets : Name.Id_List_Access);

  procedure Update_Targets;

  procedure Define_Park_Position;

  procedure Enable_Align_On_Picture;

  function In_Setup_Mode return Boolean;

  function Target_Name return String;

  procedure Show_Description (Image : String);

  function Is_Selected (The_Object : Object) return Boolean;

  function Image_Orientation return Telescope.Orientation;

end User;
