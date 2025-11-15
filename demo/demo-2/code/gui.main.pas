unit Gui.Main;

{ TTabBar Demo by Cascade/Slidescape.

  ****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
  **************************************************************************** }

{$mode objfpc}{$H+}

interface uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, CheckLst, LCLType, TabBar;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    panTabBar: TPanel;
    panSettings: TPanel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    TabBar1: TTabBar;
    TabBar2: TTabBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure TabBar1Select(Sender: TObject);
    procedure TabBar2Select(Sender: TObject);
  private
    procedure SetTabIcons;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.SetTabIcons;
begin
  TabBar1.SetIconIndexForTab(0,1);
  TabBar1.SetIconIndexForTab(1,1);
  TabBar1.SetIconIndexForTab(2,1);
  end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TabBar1Select(Sender);
  end;

procedure TForm1.TabBar1Select(Sender: TObject);
begin
  SetTabIcons;
  TabBar1.SetIconIndexForTab(TabBar1.TabIndex,0);
  end;

procedure TForm1.TabBar2Select(Sender: TObject);
begin
  PageControl1.ShowTabs:=TabBar2.TabIndex=1;
  end;

end.

