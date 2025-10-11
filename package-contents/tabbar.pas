
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

  TTabData = record
    Caption: String;
    Enabled: Boolean;
    //Icon: TBitmap;
    //IconIndex: Integer;
    end;
  TTabArray = array of TTabData;

  { TTabBar }

  TTabBar = class(TCustomControl)
  private
    FTabs: TStrings;
    FTabCount: Integer;
    FTabWidth: Double;
    FTabData: TTabArray;
    FTabIndex: Integer;
    FOnSelect: TNotifyEvent;
    procedure AttachObserver;
    //function CalculateTextCoords(ACaption: String): TPoint;
    function RoundToNearest(Value: Double): Integer;
    procedure SelectALowerTab;
    procedure SetTabEnabled(AValue: Boolean); overload;
    function GetTabEnabled: Boolean;
    function ShortenCaptionToFit(ARect: TRect; ACaption: String): String;
    function TabIsEnabled(Index: Integer): Boolean;
    procedure PaintBackground;
    procedure PaintCaption(ACaption: String; x: Integer;
      TabEnabled: Boolean=True);
    procedure PaintHighlight(x: Integer);
    procedure PaintSeparator(AIndex, x: Integer);
    procedure PaintTabs;
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
  published
    property Tabs: TStrings read FTabs write SetTabCaptions;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property TabEnabled: Boolean read GetTabEnabled write SetTabEnabled;
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
    observer.TabBar:=Self;
    end;
  end;

procedure TTabBar.PaintBackground;
begin
  Canvas.Brush.Color:=FBarBackground;
  Canvas.Pen.Color:=FBarBorder;
  Canvas.RoundRect(ClientRect,9,9);
  end;

procedure TTabBar.PaintTabs;
var
  f,iX: Integer;
  x: Double;
  sTabCaption: String;
  bTabEnabled: Boolean;
begin
  if TabCount<1 then Exit;
  FTabWidth:=Max((Width/TabCount),32);
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

{function TTabBar.CalculateTextCoords(ACaption: String): TPoint;
var
  tx: TSize;
begin
  tx:=Canvas.TextExtent(ACaption);
  Result.X:=(Trunc(FTabWidth)-tx.Width) div 2;
  Result.Y:=(Height-tx.Height) div 2;
  end;}

function TTabBar.RoundToNearest(Value: Double): Integer;
begin
  if Value.Frac<5 then Result:=Trunc(Value) else Result:=Trunc(Value)+1;
  end;

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

procedure TTabBar.PaintHighlight(x: Integer);
var
  iRight: Integer;
begin
  Canvas.Brush.Color:=FTabHighlight;
  Canvas.Pen.Color:=FBarBorder;
  iRight:=x+RoundToNearest(FTabWidth)-1;
  if iRight>Width then iRight:=Width;
  Canvas.RoundRect(Rect(x,0,iRight,Height),5,5);
  end;

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

procedure TTabBar.Paint;
begin
  PaintBackground;
  PaintTabs;
  end;

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

procedure TTabBar.SetTabCaptions(const AValue: TStrings);
begin
  if (AValue <> nil) then begin
    FTabs.Assign(AValue);
    FTabCount:=FTabs.Count;
    end;
  end;

function TTabBar.TabIsEnabled(Index: Integer): Boolean;
begin
  if {(Index<0) or} (Index>Length(FTabData)-1) then
    raise Exception.Create(Self.Name+': Index('+IntToStr(Index)+' of '
      +IntToStr(Length(FTabData))+') out of range.');
  if Index<0 then Result:=False else Result:=FTabData[Index].Enabled;
  end;

function TTabBar.GetTabEnabled: Boolean;
begin
  Result:=TabIsEnabled(TabIndex);
  end;

procedure TTabBar.SetTabEnabled(AValue: Boolean);
begin
  SetTabEnabled(TabIndex,AValue);
  end;

procedure TTabBar.SetTabEnabled(Index: Integer; AValue: Boolean);
begin
  if (Index<0) or (TabIsEnabled(Index)=AValue) then Exit;
  FTabData[Index].Enabled:=AValue;
  Invalidate;
  end;

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

procedure TTabBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  i,tw: Integer;
begin
  inherited MouseDown(Button,Shift,X,Y);
  if TabCount>0 then begin
    tw:=Max(Width div TabCount,32)+1;
    i:=X div tw;
    if (i>-1) and (i<TabCount) and (FTabData[i].Enabled) then TabIndex:=i;
    end;
  end;

procedure Register;
begin
  {$I tabbar_icon.lrs}
  RegisterComponents('Common Controls',[TTabBar]);
  end;

end.
