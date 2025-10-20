
{ TabBar Control by Cascade/Slidescape, initial revision October 2025
  
  ****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
  **************************************************************************** }
 
 unit TabBar;

{$mode ObjFPC}{$H+}

interface uses
  Classes, SysUtils, LResources, Controls, Math, Graphics, ImgList;

type

  { TTabBarDisplay}

  TTabBarDisplay=(tbdCaption,tbdCaptionAndIcon,tbdIcon);

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

  TTabPalette = record
    BarBackground: TColor;
    BarBorder: TColor;
    TabHighlight: TColor;
    TabBorder: TColor;
    TabHighlightText: TColor;
    InactiveText: TColor;
    ActiveText: TColor;
    end;

  { TPainting / TTabPainting - Temp info while control / tab is being painted. }

  TPainting = record
    DisplayMode: TTabBarDisplay;
    TabMinWidth: Integer;
    em: Integer;
    ScaleFactorMacOS: Double;
    ScalingForMSWindows: Double;
    PPIWindows: Integer;
    end;

  TTabPainting = record
    Index: Integer;
    Caption: String;
    Enabled: Boolean;
    ContentArea: TRect;
    IconWidth, IconHeight, IconX, IconY: Integer;
    end;

  { TTabBar - The TabBar control itself. }

  TTabBar = class(TCustomControl)
  private
    FDisplay: TTabBarDisplay;
    FImageWidth: Integer;
    FStyle: TTabBarStyle;
    FTabs: TStrings;
    FTabCount: Integer;
    FTabIndex: Integer;
    FTabWidth: Double;
    FTabMinWidth: Integer;
    FTabCurve: Integer;
    FTabData: TTabArray;
    FPalette: TTabPalette;
    FPainting: TPainting;
    FTabPainting: TTabPainting;
    FImages: TCustomImageList;
    FOnSelect: TNotifyEvent;
    procedure AttachObserver(Subject: TPersistent);
    procedure CalcIconPosition;
    procedure CalcScaling;
    procedure CalcTabArea(ALeft: Integer);
    procedure DrawScaledIcon(IconIndex: Integer);
    procedure ImageChangeObserved;
    procedure PaintIcon;
    function RoundToNearest(Value: Double): Integer;
    procedure PaintBackground;
    procedure PaintCaption;
    procedure PaintHighlight;
    procedure PaintSeparator;
    procedure PaintTabs;
    procedure SanitiseDisplayMode;
    procedure SelectALowerTab;
    procedure SetDisplay(AValue: TTabBarDisplay);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImageWidth(AValue: Integer);
    procedure SetStyle(AValue: TTabBarStyle);
    procedure SetTabEnabled(AValue: Boolean); overload;
    function GetTabEnabled: Boolean;
    procedure SetupColours;
    procedure ShiftCaptionPosition;
    procedure ShortenCaptionToFit(AWidth: Integer);
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
    property Display: TTabBarDisplay read FDisplay write SetDisplay;
    property ImageWidth: Integer read FImageWidth write SetImageWidth;
    property Style: TTabBarStyle read FStyle write SetStyle;
    property Tabs: TStrings read FTabs write SetTabCaptions;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property Images: TCustomImageList read FImages write SetImages;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property Enabled;
    property Font;
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
  FPaletteLight:TTabPalette=(
    BarBackground:$EAEAEA;
    BarBorder:$D5D5D5;
    TabHighlight:$FFFFFF;
    TabBorder:$CCCCCC;
    TabHighlightText:$333333;
    InactiveText:$AAAAAA;
    ActiveText:$444444);
  FPaletteDark:TTabPalette=(
    BarBackground:$444444;
    BarBorder:$535353;
    TabHighlight:$707070;
    TabBorder:$797979;
    TabHighlightText:$F0F0F0;
    InactiveText:$5F5F5F;
    ActiveText:$CECECE);

implementation uses
  Types, TextStrings, LazUtilities, LazUTF8, GraphType, Forms, TypInfo,
  DBugIntF;

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
    if (Assigned(TabBar)) then begin
      if (ASender is TTextStrings) then TabBar.TextChangeObserved
      else if (ASender is TImageList) then TabBar.ImageChangeObserved;
      end;
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
  FDisplay:=TTabBarDisplay.tbdCaptionAndIcon;
  FImageWidth:=16;
  FTabs:=TTextStrings.Create;
  AttachObserver(FTabs);
  //if FTabs.CommaText='' then FTabs.CommaText:='One,Two,Three';
  TabIndex:=-1;
  end;

destructor TTabBar.Destroy;
begin
  FreeThenNil(FTabs);
  //Do I need to free the observer here?
  end;

{ Create and attach an observer to report any changes in specified Subject. }
procedure TTabBar.AttachObserver(Subject: TPersistent);
var
  observer: TTsObserver;
  interf: IFPObserved;
begin
  if not Assigned(Subject) then Exit;
  observer:=TTsObserver.Create;
  if Supports(Subject, IFPObserved, interf) then
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
  SetupColours;
  CalcScaling;
  Canvas.Brush.Color:=FPalette.BarBackground;
  Canvas.Pen.Color:=FPalette.BarBorder;
  case FStyle of
    TTabBarStyle.tbsRectangle: FTabCurve:=0;
    TTabBarStyle.tbsRounded: FTabCurve:=Min(Height,
      RoundToNearest(5*FPainting.ScalingForMSWindows));
    end;
  Canvas.RoundRect(ClientRect,FTabCurve,FTabCurve);
  end;

procedure TTabBar.PaintTabs;
var
  f: Integer;
  x: Double;
begin
  if TabCount<1 then Exit;
  FTabWidth:=Max((Width/TabCount),FPainting.TabMinWidth);
  x:=0;
  for f:=0 to TabCount-1 do begin
    FTabPainting.Index:=f;
    FTabPainting.Caption:=FTabData[f].Caption;
    FTabPainting.Enabled:=FTabData[f].Enabled;
    FPainting.em:=Canvas.TextWidth('m');
    SanitiseDisplayMode;
    CalcTabArea(RoundToNearest(x));
    if f=TabIndex then PaintHighlight else PaintSeparator;
    FTabPainting.ContentArea.Inflate(-6,-2);
    if FPainting.DisplayMode>TTabBarDisplay.tbdCaption then PaintIcon;
    if FPainting.DisplayMode<TTabBarDisplay.tbdIcon then PaintCaption;
    x:=x+FTabWidth;
    Canvas.Brush.Color:=FPalette.BarBackground;
    end;
  end;

{ For the currently selected tab, we paint a highlight rectangle. }
procedure TTabBar.PaintHighlight;
var
  iLeft, iRight: Integer;
begin
  Canvas.Brush.Color:=FPalette.TabHighlight;
  Canvas.Pen.Color:=FPalette.TabBorder;
  iLeft:=FTabPainting.ContentArea.Left;
  iRight:=FTabPainting.ContentArea.Right;
  if iRight>Width then iRight:=Width;
  Canvas.RoundRect(Rect(iLeft,0,iRight,Height),FTabCurve,FTabCurve);
  end;

{ Round to nearest integer - we don't want bankers rounding. }
function TTabBar.RoundToNearest(Value: Double): Integer;
begin
  if Value.Frac<5 then Result:=Trunc(Value) else Result:=Trunc(Value)+1;
  end;

{ Paint a separator between unselected tabs only. }
procedure TTabBar.PaintSeparator;
var
  sSeparator: String;
  th,y, iIndex: Integer;
begin
  iIndex:=FTabPainting.Index;
  if (iIndex<1) or (iIndex=TabIndex+1) then Exit;
  sSeparator:='|';
  th:=Canvas.TextHeight(sSeparator);
  y:=(Height-th) div 2;
  Canvas.Font.Color:=FPalette.BarBorder;
  Canvas.TextOut(FTabPainting.ContentArea.Left-1,y-1,sSeparator);
  end;

{ Paint the caption inside a tab rectangle, taking into account whether the tab
  and/or control is enabled. }
procedure TTabBar.PaintCaption;
var
  ttStyle: TTextStyle;
begin
  if (Enabled) and (FTabPainting.Enabled) then begin
    if FTabPainting.Index=TabIndex
      then Canvas.Font.Color:=FPalette.TabHighlightText
      else Canvas.Font.Color:=FPalette.ActiveText;
    end
  else Canvas.Font.Color:=FPalette.InactiveText;
  if FPainting.DisplayMode=TTabBarDisplay.tbdCaption
    then ShortenCaptionToFit(FTabPainting.ContentArea.Width);
  ttStyle.Alignment:=TAlignment.taCenter;
  ttStyle.Layout:=TTextLayout.tlCenter;
  ttStyle.Clipping:=True;
  ttStyle.Opaque:=False;
  ttStyle.SingleLine:=True;
  Canvas.TextRect(FTabPainting.ContentArea,0,0,FTabPainting.Caption,ttStyle);
  end;

{ Paint the icon.  Complexity: The caption yet to be painted will need shifting
  to the right, to accomodate the icon, while the combined icon & caption remain
  centered together in the tab. A small gap is required between icon & caption,
  and gap must remain constant, independent of tab width or other measurements.

  We measure text width to calculate the desired icon x position (offset).  The
  caption rendering area's left-edge is then shifted by the width of the icon. }
procedure TTabBar.PaintIcon;
begin
  if not Assigned(FImages) then Exit;
  FTabPainting.IconWidth:=Min(FTabPainting.ContentArea.Height,
    FTabPainting.IconWidth);
  if FTabPainting.Index<FImages.Count then begin
    CalcIconPosition;
    ShiftCaptionPosition;
    DrawScaledIcon(FTabPainting.Index);
    end;
  end;

{ Paint the icon into the tab at the correct scale. }
procedure TTabBar.DrawScaledIcon(IconIndex: Integer);
begin
  FImages.DrawForPPI(Canvas, FTabPainting.IconX, FTabPainting.IconY,
    IconIndex, FTabPainting.IconWidth, FPainting.PPIWindows,
    FPainting.ScaleFactorMacOS,FTabPainting.Enabled and Enabled);
  end;

{ Determine the coords for the icon. Fairly straight forward on macOS:
  Calculations are all performed at the traditional 96dpi regardless of screen
  PPI. On MSWindows though it is up to us to incorporate screen PPI into our
  calculations... }
procedure TTabBar.CalcIconPosition;
var
  tw,offset,iw,ih,iTweak: Integer;
begin
  iTweak:=4;
  iw:=Trunc(FTabPainting.IconWidth*FPainting.ScalingForMSWindows)+iTweak;
  ih:=Trunc(FTabPainting.IconHeight*FPainting.ScalingForMSWindows)-1;
  if FTabPainting.ContentArea.Width<(iw+(4*FPainting.em))
    then FPainting.DisplayMode:=TTabBarDisplay.tbdIcon
    else ShortenCaptionToFit(FTabPainting.ContentArea.Width-iw);
  if FPainting.DisplayMode=TTabBarDisplay.tbdIcon then begin
    offset:=(FTabPainting.ContentArea.Width-iw+iTweak) div 2;
    end
  else begin
    tw:=Canvas.TextWidth(FTabPainting.Caption)+iw;
    offset:=(FTabPainting.ContentArea.Width-tw) div 2;
    end;
  FTabPainting.IconX:=FTabPainting.ContentArea.Left+offset;
  FTabPainting.IconY:=(Height-ih) div 2;
  end;

{ When painting a caption alongside an icon, we first shift the left-edge of the
  caption rendering rectangle inwards by the width of the icon. }
procedure TTabBar.ShiftCaptionPosition;
begin
  FTabPainting.ContentArea.Left:=RoundToNearest(FTabPainting.ContentArea.Left
    +((FTabPainting.IconWidth+2)*FPainting.ScalingForMSWindows));
  if Odd(FTabPainting.ContentArea.Width)
    then FTabPainting.ContentArea.Right:=FTabPainting.ContentArea.Right+1;
  end;

{ If the requested Display setting is unsuitable, e.g. Icons requested but
  no icons assigned, select an alternative. }
procedure TTabBar.SanitiseDisplayMode;
begin
  FPainting.DisplayMode:=Display;
  if Display=TTabBarDisplay.tbdCaption then Exit;
  if (Assigned(FImages)) and (FImages.Count>FTabPainting.Index) then begin
    if FTabPainting.Caption=''
      then FPainting.DisplayMode:=TTabBarDisplay.tbdIcon
      else FPainting.DisplayMode:=Display;
    end
  else FPainting.DisplayMode:=TTabBarDisplay.tbdCaption;
  FPainting.TabMinWidth:=Trunc(FTabMinWidth*FPainting.ScalingForMSWindows);
  end;

procedure TTabBar.CalcTabArea(ALeft: Integer);
var
  ARight: Integer;
begin
  ARight:=ALeft+RoundToNearest(FTabWidth);
  FTabPainting.ContentArea:=Rect(ALeft,0,ARight,Height);
  end;

{ Pre-calculate the scaling to apply to accommodate Retina/HighDPI screen pixel
  densities on macOS and Windows. }
procedure TTabBar.CalcScaling;
var
  dAspectRatio,dFactor: Double;
  iw,iHeightLimit: Integer;
begin
  if Assigned(FImages) then begin
    FPainting.PPIWindows:=Screen.PixelsPerInch;
    FPainting.ScaleFactorMacOS:=Self.GetCanvasScaleFactor;
    FPainting.ScalingForMSWindows:=FPainting.PPIWindows/96;
    dFactor:=FPainting.ScaleFactorMacOS;
    if dFactor=1 then dFactor:=FPainting.ScalingForMSWindows;
    dAspectRatio:=FImages.Width/FImages.Height;
    if FImages.Width<(FImageWidth*dFactor)
      then iw:=RoundToNearest(FImages.Width/dFactor)
      else iw:=FImageWidth;
    FTabPainting.IconWidth:=iw;
    FTabPainting.IconHeight:=RoundToNearest(iw*dAspectRatio);
    iHeightLimit:=Height-3;
    if FTabPainting.IconHeight>=iHeightLimit then begin
      dFactor:=FTabPainting.IconHeight/iHeightLimit;
      FTabPainting.IconWidth:=RoundToNearest(FTabPainting.IconWidth/dFactor);
      FTabPainting.IconHeight:=iHeightLimit;
      end;
    end;
  end;

procedure TTabBar.SetupColours;
var
  cText,cWindow:TColor;
  bDarkMode: Boolean;
begin
  cText:=ColorToRGB(clWindowText);
  cWindow:=ColorToRGB(clWindow);
  bDarkMode:=Green(cWindow)<Green(cText);
  if bDarkMode then FPalette:=FPaletteDark else FPalette:=FPaletteLight;
  end;

{ If a caption is wider than the space available on the tab, we shorten it,
  and append an ellipsis. }
procedure TTabBar.ShortenCaptionToFit(AWidth: Integer);
var
  iFitCount: Integer;
  sCaption: String;
begin
  sCaption:=FTabPainting.Caption;
  if UTF8Length(sCaption)>1 then begin
    iFitCount:=Canvas.TextFitInfo(sCaption,AWidth);
    if iFitCount<UTF8Length(sCaption)
      then FTabPainting.Caption:=Trim(sCaption.Remove(iFitCount-2))+'…';
    end;
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

{ The observer attached to FImages has reported a change, so we Invalidate the
  control so that it will repaint with whatever has changed in FImages. }
procedure TTabBar.ImageChangeObserved;
begin
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

{ Setter for Display.  Display caption, icons or both. }
procedure TTabBar.SetDisplay(AValue: TTabBarDisplay);
begin
  if FDisplay=AValue then Exit;
  FDisplay:=AValue;
  Invalidate;
  end;

{ Setter for Images property. IDE calls this procedure when an ImageList is
  assigned to the control. }
procedure TTabBar.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then Exit;
  FImages := AValue;
  AttachObserver(FImages);
  Invalidate;
  end;

{ Setter for ImageWidth - the requested width of an icon. }
procedure TTabBar.SetImageWidth(AValue: Integer);
begin
  if FImageWidth=AValue then Exit;
  if AValue>=8 then begin
    FImageWidth:=AValue;
    Invalidate;
    end;
  end;

{ Setter for Style property. Sets the display style of the TabBar control,
  e.g. tbsRounded. }
procedure TTabBar.SetStyle(AValue: TTabBarStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  Invalidate;
  end;

{ Setter for Tabs property. Assigns new tab captions. Called by the IDE Form
  Designer Strings editor. }
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
  if Index=-1 then Result:=False else begin
    if (Index<-1) or (Index>Length(FTabData)-1) then
    raise Exception.Create(Self.Name+': Index('+IntToStr(Index)+' of '
      +IntToStr(Length(FTabData))+') out of range.');
    Result:=FTabData[Index].Enabled;
    end;
  end;

{ Getter for TabEnabled property. Returns True if the current tab is Enabled. }
function TTabBar.GetTabEnabled: Boolean;
begin
  Result:=TabIsEnabled(TabIndex);
  end;

{ Setter for TabEnabled property. Sets the Enabled state of the current tab. }
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

{ Setter for TabIndex property. Selects the specified tab, making it the current
  highlighted tab. If the specified tab index is too high, the end tab is
  selected. Specify -1 to have no tab selected. Note that unlike clicking on a
  tab, this method can be called to force select a tab that is not currently
  Enabled. }
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
    tw:=Max(Width div TabCount,FPainting.TabMinWidth)+1;
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
