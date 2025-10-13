
{ TabBar Control by Cascade/Slidescape, initial revision October 2025
  
  ****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
  **************************************************************************** }
 
 unit TabBar;

{$mode ObjFPC}{$H+}

interface uses
  Classes, SysUtils, LResources, Controls, Math, Graphics;

type

  { TTabBarStyle }

  TTabBarStyle=(tbsRectangle,tbsRounded);

  { TTabData - Data per tab. }

  TTabData = record
    Caption: String;
    Enabled: Boolean;
    //Icon: TBitmap;
    //IconIndex: Integer;
    end;
  TTabArray = array of TTabData;

  { TTabBar - The TabBar control itself. }

  TTabBar = class(TCustomControl)
  private
    FStyle: TTabBarStyle;
    FTabs: TStrings;
    FTabCount: Integer;
    FTabIndex: Integer;
    FTabWidth: Double;
    FTabMinWidth: Integer;
    FTabCurve: Integer;
    FTabData: TTabArray;
    FOnSelect: TNotifyEvent;
    procedure AttachObserver;
    function RoundToNearest(Value: Double): Integer;
    procedure PaintBackground;
    procedure PaintCaption(ACaption: String; x: Integer;
      TabEnabled: Boolean=True);
    procedure PaintHighlight(x: Integer);
    procedure PaintSeparator(AIndex, x: Integer);
    procedure PaintTabs;
    procedure SelectALowerTab;
    procedure SetStyle(AValue: TTabBarStyle);
    procedure SetTabEnabled(AValue: Boolean); overload;
    function GetTabEnabled: Boolean;
    function ShortenCaptionToFit(ARect: TRect; ACaption: String): String;
    function TabIsEnabled(Index: Integer): Boolean;
  protected
    procedure Paint; override;
    procedure TextChangeObserved; virtual;
    procedure SetTabCaptions(const AValue: TStrings); virtual;
    procedure SetTabIndex(Value: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetTabEnabled(Index: Integer; AValue: Boolean); overload;
    property TabCount: Integer read FTabCount;
    property TabEnabled: Boolean read GetTabEnabled write SetTabEnabled;
  published
    property Style: TTabBarStyle read FStyle write SetStyle;
    property Tabs: TStrings read FTabs write SetTabCaptions;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property Enabled;
    property Font;
    //property Images;
    //property ImagesWidth;
    property Constraints;
    property ShowHint;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    // events
    property OnContextPopup;
    property OnClick;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

  { TTsObserver - TStrings observer to detect any changes to tab captions. }

  TTsObserver = class(TObject,IFPObserver)
  public
    TabBar: TTabBar;
  private
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);
    function GetOperationName(AOp: TFPObservedOperation): String;
  end;

procedure Register;

var
  FBarBackground: Integer = $DDDDDD;
  FBarBorder: Integer = $CCCCCC;
  FTabHighlight: Integer = $FFFFFF;
  FInactiveText: Integer = $BBBBBB;
  FActiveText: Integer = $333333;

implementation uses
  Types, TextStrings, LazUtilities, LazUTF8, TypInfo, DBugIntF;

{ TTsObserver }

function TTsObserver.GetOperationName(AOp: TFPObservedOperation): String;
begin
  Result:=GetEnumName(TypeInfo(TFPObservedOperation),Ord(AOp));
  end;

procedure TTsObserver.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
var
  interf: IFPObserved;
begin
  case Operation of
  ooFree:
    if Supports(ASender,IFPObserved,interf) then
      interf.FPODetachObserver(Self);
  ooChange:
    if (Assigned(TabBar)) and (ASender is TTextStrings)
      then TabBar.TextChangeObserved;
  else
    SendDebug('Observed a change in '+ASender.ClassName+'; '
      +GetOperationName(Operation));
    end;
  end;

{ TTabBar }

constructor TTabBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width:=250;
  Height:=25;
  FTabWidth:=60;
  FTabMinWidth:=32;
  FTabCurve:=5;
  FTabs:=TTextStrings.Create;
  AttachObserver;
  //if FTabs.CommaText='' then FTabs.CommaText:='One,Two,Three';
  TabIndex:=-1;
  end;

destructor TTabBar.Destroy;
begin
  FreeThenNil(FTabs);
  //Do I need to free the observer here?
  end;

{ Create and attach an observer to report any changes to caption strings. }
procedure TTabBar.AttachObserver;
var
  subject: TStrings;
  observer: TTsObserver;
  interf: IFPObserved;
begin
  subject:=FTabs;
  observer:=TTsObserver.Create;
  if Supports(subject, IFPObserved, interf) then
  begin
    interf.FPOAttachObserver(observer); //Attach observer to subject interface.
    observer.TabBar:=Self;              //Observer will need to notify TabBar.
    end;
  end;

{ Methods to paint the TabBar & Tabs... }

procedure TTabBar.Paint;
begin
  PaintBackground;
  PaintTabs;
  end;

procedure TTabBar.PaintBackground;
begin
  Canvas.Brush.Color:=FBarBackground;
  Canvas.Pen.Color:=FBarBorder;
  case FStyle of
    TTabBarStyle.tbsRectangle: FTabCurve:=0;
    TTabBarStyle.tbsRounded: FTabCurve:=5;
    end;
  Canvas.RoundRect(ClientRect,FTabCurve,FTabCurve);
  end;

procedure TTabBar.PaintTabs;
var
  f,iX: Integer;
  x: Double;
  sTabCaption: String;
  bTabEnabled: Boolean;
begin
  if TabCount<1 then Exit;
  FTabWidth:=Max((Width/TabCount),FTabMinWidth);
  x:=0;
  for f:=0 to TabCount-1 do begin
    iX:=RoundToNearest(x);
    sTabCaption:=FTabData[f].Caption;
    bTabEnabled:=FTabData[f].Enabled;
    if f=TabIndex then PaintHighlight(iX)
                  else PaintSeparator(f,iX);
    PaintCaption(sTabCaption,iX,bTabEnabled);
    x:=x+FTabWidth;
    Canvas.Brush.Color:=FBarBackground;
    end;
  end;

{ For the currently selected tab, we paint a highlight rectangle. }
procedure TTabBar.PaintHighlight(x: Integer);
var
  iRight: Integer;
begin
  Canvas.Brush.Color:=FTabHighlight;
  Canvas.Pen.Color:=FBarBorder;
  iRight:=x+RoundToNearest(FTabWidth)-1;
  if iRight>Width then iRight:=Width;
  Canvas.RoundRect(Rect(x,0,iRight,Height),FTabCurve,FTabCurve);
  end;

{ Round to nearest integer - we don't want bankers rounding. }
function TTabBar.RoundToNearest(Value: Double): Integer;
begin
  if Value.Frac<5 then Result:=Trunc(Value) else Result:=Trunc(Value)+1;
  end;

{ Paint a separator between unselected tabs only. }
procedure TTabBar.PaintSeparator(AIndex,x: Integer);
var
  sSeparator: String;
  th,y: Integer;
begin
  if (AIndex<1) or (AIndex=TabIndex+1) then Exit;
  sSeparator:='|';
  th:=Canvas.TextHeight(sSeparator);
  y:=(Height-th) div 2;
  Canvas.Font.Color:=FBarBorder;
  Canvas.TextOut(x-1,y-1,sSeparator);
  end;

{ Paint the caption inside a tab rectangle, taking into account whether the tab
  and/or control is enabled. }
procedure TTabBar.PaintCaption(ACaption: String; x: Integer;
  TabEnabled: Boolean=True);
var
  rtTab: TRect;
  ttStyle: TTextStyle;
  iTabPadding: Integer;
begin
  if (Enabled) and (TabEnabled) then Canvas.Font.Color:=FActiveText
                                else Canvas.Font.Color:=FInactiveText;
  iTabPadding:=5;
  rtTab:=Rect(x+iTabPadding,0,x+Trunc(FTabWidth)-iTabPadding,Height);
  ACaption:=ShortenCaptionToFit(rtTab,ACaption);
  ttStyle.Alignment:=TAlignment.taCenter;
  ttStyle.Layout:=TTextLayout.tlCenter;
  ttStyle.Clipping:=True;
  ttStyle.Opaque:=False;
  ttStyle.SingleLine:=True;
  Canvas.TextRect(rtTab,0,0,ACaption,ttStyle);
  end;

{ If a caption is wider than the space available on the tab, we shorten it,
  and append an ellipsis. }
function TTabBar.ShortenCaptionToFit(ARect: TRect; ACaption: String): String;
var
  iFitCount: Integer;
begin
  if UTF8Length(ACaption)>1 then begin
    iFitCount:=Canvas.TextFitInfo(ACaption,ARect.Width);
    if iFitCount<UTF8Length(ACaption)
      then ACaption:=ACaption.Remove(iFitCount-2)+'…';
    end;
  Result:=ACaption;
  end;

{ The observer attached to FTabs has reported a change in the caption strings,
  so we update our tab data and repaint. }
procedure TTabBar.TextChangeObserved;
var
  OldCount,f: Integer;
begin
  OldCount:=FTabCount;
  FTabCount:=Tabs.Count;
  if TabIndex>=FTabCount then SelectALowerTab;
  SetLength(FTabData,TabCount);
  for f:=0 to TabCount-1 do begin
    FTabData[f].Caption:=FTabs[f];
    if f>=OldCount then FTabData[f].Enabled:=True;
    end;
  Invalidate;
  end;

{ If the currently selected tab index no longer exists i.e. tab count has been
  reduced - we select a lower tab index, by picking the highest available
  (enabled) tab, or -1 if there are no enabled tabs. }
procedure TTabBar.SelectALowerTab;
var
  f,i: Integer;
begin
  i:=-1;
  for f:=FTabCount-1 downto 0 do begin
    if TabIsEnabled(f) then begin
      i:=f;
      Break;
      end;
    end;
  SetTabIndex(i);
  end;

{ Set the display style of the TabBar control, e.g. tbsRounded. }
procedure TTabBar.SetStyle(AValue: TTabBarStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  Invalidate;
  end;

{ Assign new tab captions - called by the IDE Form Designer Strings editor. }
procedure TTabBar.SetTabCaptions(const AValue: TStrings);
begin
  if (AValue <> nil) then begin
    FTabs.Assign(AValue);
    FTabCount:=FTabs.Count;
    end;
  end;

{ Returns True if the specified tab is Enabled. }
function TTabBar.TabIsEnabled(Index: Integer): Boolean;
begin
  if {(Index<0) or} (Index>Length(FTabData)-1) then
    raise Exception.Create(Self.Name+': Index('+IntToStr(Index)+' of '
      +IntToStr(Length(FTabData))+') out of range.');
  if Index<0 then Result:=False else Result:=FTabData[Index].Enabled;
  end;

{ Returns True if the current tab is Enabled. }
function TTabBar.GetTabEnabled: Boolean;
begin
  Result:=TabIsEnabled(TabIndex);
  end;

{ Sets the Enabled state of the current tab. }
procedure TTabBar.SetTabEnabled(AValue: Boolean);
begin
  SetTabEnabled(TabIndex,AValue);
  end;

{ Sets the Enabled state of the specified tab. }
procedure TTabBar.SetTabEnabled(Index: Integer; AValue: Boolean);
begin
  if (Index<0) or (TabIsEnabled(Index)=AValue) then Exit;
  FTabData[Index].Enabled:=AValue;
  Invalidate;
  end;

{ Select the specified tab, making it the current highlighted tab. If the
  specified tab index is too high, the highest tab is selected. Specify -1 to
  have no tab selected. Note that unlike clicking on a tab, this method can
  be called to force select a tab that is not currently Enabled. }
procedure TTabBar.SetTabIndex(Value: Integer);
begin
  if FTabIndex<>Value then begin
    if Value>=TabCount then Value:=TabCount-1
    else if Value<-1 then Value:=-1;
    FTabIndex:=Value;
    Invalidate;
    if Assigned(OnSelect) then OnSelect(Self);
    end;
  end;

{ The TabBar has been clicked. Determine which tab was clicked and select it.
  Setting TabIndex to a new value Invalidates the control so it will repaint. }
procedure TTabBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i,tw: Integer;
begin
  inherited MouseDown(Button,Shift,X,Y);
  if TabCount>0 then begin
    tw:=Max(Width div TabCount,FTabMinWidth)+1;
    i:=X div tw;
    if (i>-1) and (i<TabCount) and (FTabData[i].Enabled) then TabIndex:=i;
    end;
  end;

{function TTabBar.CalculateTextCoords(ACaption: String): TPoint;
var
  tx: TSize;
begin
  tx:=Canvas.TextExtent(ACaption);
  Result.X:=(Trunc(FTabWidth)-tx.Width) div 2;
  Result.Y:=(Height-tx.Height) div 2;
  end;}

procedure Register;
begin
  {$I tabbar_icon.lrs}
  RegisterComponents('Common Controls',[TTabBar]);
  end;

end.
