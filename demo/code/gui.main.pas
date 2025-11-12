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
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    btnDeleteTab: TButton;
    btnAddNewTab: TButton;
    cbxAccent: TComboBox;
    cbxStyle: TComboBox;
    cbxDisplayMode: TComboBox;
    cbxHeight: TComboBox;
    clbTabs: TCheckListBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    ImageList1: TImageList;
    ImageList2: TImageList;
    ImageList3: TImageList;
    labHighlight: TLabel;
    labIconSet: TLabel;
    labHeight: TLabel;
    labTabList: TLabel;
    labEditCaption: TLabel;
    labStyling: TLabel;
    labDisplayMode: TLabel;
    panTabBar: TPanel;
    panSettings: TPanel;
    panDisplaySettings: TPanel;
    panStyleSettings: TPanel;
    panCaptions: TPanel;
    Edit1: TEdit;
    TabBar1: TTabBar;
    procedure btnDeleteTabClick(Sender: TObject);
    procedure btnAddNewTabClick(Sender: TObject);
    procedure cbxAccentChange(Sender: TObject);
    procedure cbxDisplayModeChange(Sender: TObject);
    procedure cbxHeightChange(Sender: TObject);
    procedure cbxStyleChange(Sender: TObject);
    procedure clbTabsClickCheck(Sender: TObject);
    procedure clbTabsSelectionChange(Sender: TObject; User: boolean);
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure TabBar1Select(Sender: TObject);
  private
    procedure RefreshTabList;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cbxStyleChange(Sender: TObject);
var
  i:Integer;
begin
  i:=cbxStyle.ItemIndex;
  if i>-1 then TabBar1.Style:=TTabBarStyle(i);
  end;

procedure TForm1.clbTabsClickCheck(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to clbTabs.Count-1 do
    TabBar1.SetTabEnabled(i,clbTabs.Checked[i]);
  end;

procedure TForm1.clbTabsSelectionChange(Sender: TObject; User: boolean);
var
  i: Integer;
begin
  i:=clbTabs.ItemIndex;
  if i<0 then Exit;
  if User then TabBar1.TabIndex:=i;
  Edit1.Text:=TabBar1.Tabs[i];
  end;

procedure TForm1.Edit1Change(Sender: TObject);
var
  i: Integer;
begin
  i:=TabBar1.TabIndex;
  if i<0 then Exit;
  TabBar1.Tabs[i]:=Trim(Edit1.Text);
  clbTabs.Items[i]:=TabBar1.Tabs[i];
  end;

procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  i: Integer;
begin
  if Key=VK_RETURN then begin
    i:=TabBar1.TabIndex;
    if ssShift in Shift then begin
      Dec(i);
      if i<0 then i:=TabBar1.TabCount-1;
      end
    else begin
      Inc(i);
      if i>=TabBar1.TabCount then i:=0;
      end;
    TabBar1.TabIndex:=i;
    end;
  end;

procedure TForm1.FormShow(Sender: TObject);
begin
  clbTabs.CheckAll(TCheckBoxState.cbChecked);
  end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  TabBar1.Images:=ImageList1;
  end;

procedure TForm1.Image2Click(Sender: TObject);
begin
  TabBar1.Images:=ImageList2;
  end;

procedure TForm1.Image3Click(Sender: TObject);
begin
  TabBar1.Images:=ImageList3;
  end;

procedure TForm1.RefreshTabList;
var
  i,iTop: Integer;
begin
  clbTabs.Items.BeginUpdate;
  try
    iTop:=TabBar1.Tabs.Count-1;
    while clbTabs.Count<=iTop do clbTabs.Items.Add('');
    while clbTabs.Count>iTop+1 do clbTabs.Items.Delete(clbTabs.Count-1);
    for i:=0 to iTop do begin
      clbTabs.Items[i]:=TabBar1.Tabs[i];
      clbTabs.Checked[i]:=TabBar1.TabIsEnabled(i);
      end;
  finally
    clbTabs.Items.EndUpdate;
    end;
  end;

procedure TForm1.btnDeleteTabClick(Sender: TObject);
var
  i: Integer;
begin
  i:=TabBar1.TabIndex;
  if i<0 then Exit;
  TabBar1.Tabs.Delete(i);
  RefreshTabList;
  end;

procedure TForm1.btnAddNewTabClick(Sender: TObject);
var
  i,j: Integer;
  sCaption: String;
begin
  i:=TabBar1.TabIndex+1;
  j:=1;
  repeat
    sCaption:='Tab '+IntToStr(j);
    Inc(j);
    until TabBar1.Tabs.IndexOf(sCaption)<0;
  TabBar1.Tabs.Insert(i,sCaption);
  RefreshTabList;
  TabBar1.TabIndex:=i;
  end;

procedure TForm1.cbxAccentChange(Sender: TObject);
var
  i: Integer;
begin
  i:=cbxAccent.ItemIndex;
  if i<0 then Exit;
  TabBar1.AccentColor:=TTabBarAccent(i);
  end;

procedure TForm1.cbxDisplayModeChange(Sender: TObject);
var
  i:Integer;
begin
  i:=cbxDisplayMode.ItemIndex;
  if i>-1 then TabBar1.Display:=TTabBarDisplay(i);
  end;

procedure TForm1.cbxHeightChange(Sender: TObject);
var
  i,h:Integer;
begin
  i:=cbxHeight.ItemIndex;
  if i=1 then h:=64 else h:=32;
  panTabBar.Height:=panTabBar.Scale96ToForm(h);
  end;

procedure TForm1.TabBar1Select(Sender: TObject);
begin
  clbTabs.ItemIndex:=TabBar1.TabIndex;
  end;

end.

