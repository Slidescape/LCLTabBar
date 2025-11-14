
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

  TTabBarStyle=(tbsRectangle,tbsSoftRect,tbsRounded,tbsLozenge);

  { TTabBarAccent }

  TTabBarAccent=(tbaBright,tbaHot,tbaLight);

  { TTabData - Data per tab. }

  TTabData = record
    Caption: String;
    Enabled: Boolean;
    IconIndex: Integer;
    //Icon: TBitmap;
    end;
  TTabArray = array of TTabData;

  { TTabList}

  TTabList = class(TStringlist)
  private
    FTabData: TTabArray;
    function NewTabData(S: String):TTabData;
  protected
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    property Data: TTabArray read FTabData;
    procedure Delete(Index: Integer); override;
    end;

  TTabPalette = record
    BarBackground: TColor;
    BarBorder: TColor;
    TabHighlight: TColor;
    TabBorder: TColor;
    TabHighlightText: TColor;
    InactiveText: TColor;
    ActiveText: TColor;
    InverseText: TColor;
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
    FBorder: Boolean;
    FDisplay: TTabBarDisplay;
    FImageWidth: Integer;
    FStyle: TTabBarStyle;
    FAccent: TTabBarAccent;
    FTabs: TStrings;
    FTabCount: Integer;
    FTabIndex: Integer;
    FTabWidth: Double;
    FTabMinWidth: Integer;
    FTabCurve: Integer;
    FPalette: TTabPalette;
    FPainting: TPainting;
    FTabPainting: TTabPainting;
    FImages: TCustomImageList;
    FOnSelect: TNotifyEvent;
    procedure AttachObserver(Subject: TPersistent);
    procedure CalcTabLayout;
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
    procedure SanitiseIconWidth;
    procedure SelectALowerTab;
    procedure SetBorder(AValue: Boolean);
    procedure SetDisplay(AValue: TTabBarDisplay);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImageWidth(AValue: Integer);
    procedure SetStyle(AValue: TTabBarStyle);
    procedure SetAccent(AValue: TTabBarAccent);
    procedure SetTabEnabled(AValue: Boolean); overload;
    function GetTabEnabled: Boolean;
    procedure SetupColours;
    procedure SetupHighlightColour;
    procedure SetupPalette;
    procedure ShiftCaptionHorizontal;
    procedure ShortenCaptionToFit(AWidth: Integer);
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
    function TabIsEnabled(Index: Integer): Boolean;
    procedure SetIconIndexForTab(AIndex, AIconIndex: Integer);
    function IconIndexForTab(AIndex: Integer): Integer;
    function IconIndexResolvedForTab(AIndex: Integer): Integer;
    property TabCount: Integer read FTabCount;
    property TabEnabled: Boolean read GetTabEnabled write SetTabEnabled;
  published
    property Border: Boolean read FBorder write SetBorder;
    property Display: TTabBarDisplay read FDisplay write SetDisplay;
    property ImageWidth: Integer read FImageWidth write SetImageWidth;
    property Style: TTabBarStyle read FStyle write SetStyle;
    property AccentColor: TTabBarAccent read FAccent write SetAccent;
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
    TabBorder:$CACACA;
    TabHighlightText:$333333;
    InactiveText:$AAAAAA;
    ActiveText:$444444;
    InverseText:$FCFCFC);
  FPaletteDark:TTabPalette=(
    BarBackground:$444444;
    BarBorder:$535353;
    TabHighlight:$707070;
    TabBorder:$797979;
    TabHighlightText:$F0F0F0;
    InactiveText:$5F5F5F;
    ActiveText:$CECECE;
    InverseText:$333333);

implementation uses
  Types, LazUtilities, LazUTF8, GraphType, Forms, TypInfo, DBugIntF;

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
      if (ASender is TStringlist) then TabBar.TextChangeObserved
      else if (ASender is TImageList) then TabBar.ImageChangeObserved;
      end;
  else
    SendDebug('Observed a change in '+ASender.ClassName+'; '
      +GetOperationName(Operation));
    end;
  end;

{ TTabList }

function TTabList.NewTabData(S: String):TTabData;
begin
  Result.Caption:=S;
  Result.Enabled:=True;
  Result.IconIndex:=-1;
  end;

procedure TTabList.InsertItem(Index: Integer; const S: string; O: TObject);
begin
  inherited InsertItem(Index, S, O);
  System.Insert(NewTabData(S),FTabData,Index);
  end;

procedure TTabList.Delete(Index: Integer);
begin
  System.Delete(FTabData,Index,1);
  inherited Delete(Index);
  end;

{ TTabBar }

constructor TTabBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width:=250;
  Height:=25;
  FTabWidth:=60;
  FTabMinWidth:=32;
  FBorder:=True;
  FStyle:=TTabBarStyle.tbsRectangle;
  FTabCurve:=5;
  FDisplay:=TTabBarDisplay.tbdCaptionAndIcon;
  FImageWidth:=16;
  FTabs:=TTablist.Create;
  AttachObserver(FTabs);
  TabIndex:=-1;
  Canvas.AntialiasingMode:=TAntialiasingMode.amOn;
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
  if FBorder then Canvas.Pen.Color:=FPalette.BarBorder
             else Canvas.Pen.Color:=FPalette.BarBackground;
  case FStyle of
    TTabBarStyle.tbsRectangle: FTabCurve:=0;
    TTabBarStyle.tbsSoftRect: FTabCurve:=Min(Height,
      RoundToNearest(5*FPainting.ScalingForMSWindows));
    TTabBarStyle.tbsRounded: FTabCurve:=Min(Height,
      RoundToNearest(12*FPainting.ScalingForMSWindows));
    TTabBarStyle.tbsLozenge: FTabCurve:=Height;
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
    FTabPainting.Caption:=TTabList(FTabs).FTabData[f].Caption;
    FTabPainting.Enabled:=TTabList(FTabs).FTabData[f].Enabled;
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

{ Paint the icon.  Complexity: The caption yet to be painted will need shifting,
  to accomodate the icon, while the combined icon & caption remain centered
  together in the tab. A small gap is required between icon & caption, and the
  gap must remain constant, independent of tab size or other measurements.

  We measure text width to calculate the desired icon x position (offset).  The
  caption rendering area's left-edge is then shifted by the width of the icon. }
procedure TTabBar.PaintIcon;
var
  iTabIndex,iIconIndex: Integer;
begin
  if not Assigned(FImages) then Exit;
  iTabIndex:=FTabPainting.Index;
  iIconIndex:=IconIndexResolvedForTab(iTabIndex);
  if iIconIndex<0 then iIconIndex:=iTabIndex;
  if iIconIndex>=FImages.Count then Exit;
  SanitiseIconWidth;
  CalcTabLayout;
  DrawScaledIcon(iIconIndex);
  end;

{ Paint the icon into the tab at the correct scale. }
procedure TTabBar.DrawScaledIcon(IconIndex: Integer);
begin
  FImages.DrawForPPI(Canvas, FTabPainting.IconX, FTabPainting.IconY,
    IconIndex, FTabPainting.IconWidth, FPainting.PPIWindows,
    FPainting.ScaleFactorMacOS,FTabPainting.Enabled and Enabled);
  end;

{ Determine the layout for icon and/or caption. On macOS calculations are all
  performed at the traditional 96dpi regardless of screen PPI. On MSWindows
  though it is up to us to incorporate screen PPI into our calculations.

  Layout factors include: Caption/Partial Caption only, Icon only, Icon
  alongside Caption/Partial Caption, Icon above Caption/Partial Caption. }
procedure TTabBar.CalcTabLayout;
var
  tw,th,offset,iw,ih,xw,iTweak: Integer;
  bIconAbove: Boolean;
begin
  iTweak:=4;
  iw:=Trunc(FTabPainting.IconWidth*FPainting.ScalingForMSWindows)+iTweak;
  ih:=Trunc(FTabPainting.IconHeight*FPainting.ScalingForMSWindows)-1;
  th:=Canvas.TextHeight(FTabPainting.Caption)+iTweak;
  bIconAbove:=(FPainting.DisplayMode=TTabBarDisplay.tbdCaptionAndIcon)
    and (th+ih<Height);
  if bIconAbove then xw:=0 else xw:=iw;
  { If there isn't space for a decent fragment of caption, show Icon only. }
  if FTabPainting.ContentArea.Width<(xw+(4*FPainting.em)) then begin
    FPainting.DisplayMode:=TTabBarDisplay.tbdIcon;
    bIconAbove:=False;
    end
  else begin
    ShortenCaptionToFit(FTabPainting.ContentArea.Width-xw);
    if bIconAbove then Inc(FTabPainting.ContentArea.Top,ih+iTweak);
    end;
  if (FPainting.DisplayMode=TTabBarDisplay.tbdIcon) or (bIconAbove) then begin
    offset:=(FTabPainting.ContentArea.Width-iw+iTweak) div 2;
    end
  else begin
    tw:=Canvas.TextWidth(FTabPainting.Caption)+iw;
    offset:=(FTabPainting.ContentArea.Width-tw) div 2;
    end;
  FTabPainting.IconX:=FTabPainting.ContentArea.Left+offset;
  if bIconAbove then FTabPainting.IconY:=(Height-ih-th+iTweak) div 2
  else begin
    FTabPainting.IconY:=(Height-ih) div 2;
    ShiftCaptionHorizontal;
    end;
  end;

{ When painting a caption alongside an icon, we first shift the left-edge of the
  caption rendering rectangle inwards by the width of the icon. }
procedure TTabBar.ShiftCaptionHorizontal;
begin
  FTabPainting.ContentArea.Left:=RoundToNearest(FTabPainting.ContentArea.Left
    +((FTabPainting.IconWidth+2)*FPainting.ScalingForMSWindows));
  if Odd(FTabPainting.ContentArea.Width)
    then FTabPainting.ContentArea.Right:=FTabPainting.ContentArea.Right+1;
  end;

{ Ensure icon is not larger than the available height. }
procedure TTabBar.SanitiseIconWidth;
begin
  FTabPainting.IconWidth:=Min(FTabPainting.ContentArea.Height,
    FTabPainting.IconWidth);
  end;

{ If the requested Display setting is unsuitable, e.g. Icons requested but
  no icons assigned, select an alternative. }
procedure TTabBar.SanitiseDisplayMode;
begin
  FPainting.DisplayMode:=Display;
  if Display=TTabBarDisplay.tbdCaption then Exit;
  if (Assigned(FImages))
  and (FImages.Count>IconIndexResolvedForTab(FTabPainting.Index)) then begin
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
  iw,ih,iHeightLimit: Integer;
begin
  FPainting.PPIWindows:=Screen.PixelsPerInch;
  FPainting.ScaleFactorMacOS:=Self.GetCanvasScaleFactor;
  FPainting.ScalingForMSWindows:=FPainting.PPIWindows/96;
  if Assigned(FImages) then begin
    dFactor:=FPainting.ScaleFactorMacOS;
    if dFactor=1 then dFactor:=FPainting.ScalingForMSWindows;
    dAspectRatio:=FImages.Width/FImages.Height;
    if FImages.Width<(FImageWidth*dFactor)
      then iw:=RoundToNearest(FImages.Width/dFactor)
      else iw:=FImageWidth;
    FTabPainting.IconWidth:=iw;
    FTabPainting.IconHeight:=RoundToNearest(iw*dAspectRatio);
    ih:=Trunc(Height/FPainting.ScalingForMSWindows);
    iHeightLimit:=ih-3;
    if FTabPainting.IconHeight>=iHeightLimit then begin
      dFactor:=FTabPainting.IconHeight/iHeightLimit;
      FTabPainting.IconWidth:=RoundToNearest(FTabPainting.IconWidth/dFactor);
      FTabPainting.IconHeight:=iHeightLimit;
      end;
    end;
  end;

procedure TTabBar.SetupColours;
begin
  SetupPalette;
  SetupHighlightColour;
  end;

{ Select the light or dark palette by testing clWindow against clWindowText
  to see whether system is in Dark Mode. This works on macOS, but MSWindows
  LCL widgetset may not support dark mode yet. }

procedure TTabBar.SetupPalette;
var
  cText,cWindow: TColor;
  bDarkMode: Boolean;
begin
  cText:=ColorToRGB(clWindowText);
  cWindow:=ColorToRGB(clWindow);
  bDarkMode:=Green(cWindow)<Green(cText);
  if bDarkMode then FPalette:=FPaletteDark else FPalette:=FPaletteLight;
  end;

{ Set the Highlight Accent colour, then test its luminosity and select the text
  colour that gives greatest contrast. }
procedure TTabBar.SetupHighlightColour;
var
  i0,i1,i2: Integer;
begin
  if FAccent<=TTabBarAccent.tbaBright then Exit;
  case FAccent of
    TTabBarAccent.tbaHot: FPalette.TabHighlight:=clHotLight;
    TTabBarAccent.tbaLight: FPalette.TabHighlight:=clHighlight;
    end;
  i0:=Green(ColorToRGB(FPalette.TabHighlight));
  i1:=Abs(i0-Green(FPalette.ActiveText));
  i2:=Abs(i0-Green(FPalette.InverseText));
  if i1<i2 then FPalette.TabHighlightText:=FPalette.InverseText;
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
  so we size our tab data array, adjust TabIndex if necessary, and repaint. }
procedure TTabBar.TextChangeObserved;
var
  i: Integer;
begin
  FTabCount:=Tabs.Count;
  SetLength(TTabList(FTabs).FTabData,FTabCount);
  for i:=0 to FTabCount-1 do TTabList(FTabs).FTabData[i].Caption:=FTabs[i];
  if (TabIndex>=FTabCount) or (not TTabList(FTabs).FTabData[TabIndex].Enabled)
    then SelectALowerTab;
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
  for f:=FTabIndex-1 downto 0 do begin
    if TabIsEnabled(f) then begin
      i:=f;
      Break;
      end;
    end;
  SetTabIndex(i);
  end;

{ Setter for Border.  Display control with a border, or borderless. }

procedure TTabBar.SetBorder(AValue: Boolean);
begin
  if FBorder=AValue then Exit;
  FBorder:=AValue;
  Invalidate;
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

{ Setter for AccentColor property. Sets the accent colour used to highlight
  the currently active tab. }

procedure TTabBar.SetAccent(AValue: TTabBarAccent);
begin
  if FAccent=AValue then Exit;
  FAccent:=AValue;
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
    if (Index<-1) or (Index>Length(TTabList(FTabs).FTabData)-1) then
    raise Exception.Create(Self.Name+': Index('+IntToStr(Index)+' of Count '
      +IntToStr(Length(TTabList(FTabs).FTabData))+') out of range.');
    Result:=TTabList(FTabs).FTabData[Index].Enabled;
    end;
  end;

procedure TTabBar.SetIconIndexForTab(AIndex, AIconIndex: Integer);
var
  i: Integer;
begin
  i:=IconIndexForTab(AIndex);
  if i=AIconIndex then Exit;
  TTabList(FTabs).FTabData[AIndex].IconIndex:=AIconIndex;
  Invalidate;
  end;

{ Returns the explicit icon index for the specific tab, or -1 if no explicit
  icon index assigned. -1 indicates the default auto icon index applies. }
function TTabBar.IconIndexForTab(AIndex: Integer): Integer;
begin
  if (AIndex<0) or (AIndex>=TabCount)
    then raise Exception.Create('Index out of range: '+IntToStr(AIndex));
  Result:=TTabList(FTabs).FTabData[AIndex].IconIndex;
  end;

{ Same as IconIndexForTab, except if no explicit icon index assigned (i.e. -1)
  then it resolves the actual default auto icon index i.e. matches tab index. }
function TTabBar.IconIndexResolvedForTab(AIndex: Integer): Integer;
begin
  Result:=IconIndexForTab(AIndex);
  if Result<0 then Result:=AIndex; //Default icon index equals index of the tab.
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
  TTabList(FTabs).FTabData[Index].Enabled:=AValue;
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
  i: Integer;
  tw: Double;
begin
  inherited MouseDown(Button,Shift,X,Y);
  if TabCount>0 then begin
    tw:=Max(Width,FPainting.TabMinWidth*TabCount);
    i:=RoundToNearest((X*TabCount)/tw)-1;
    if (i>-1) and (i<TabCount) and (TTabList(FTabs).FTabData[i].Enabled)
      then TabIndex:=i;
    end;
  end;

procedure Register;
begin
  {$I tabbar_icon.lrs}
  RegisterComponents('Common Controls',[TTabBar]);
  end;

end.
