unit _ACEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls;

const
 IID_AutoComplete: TGUID = '{00bb2762-6a77-11d0-a535-00c04fd7d062}';
 IID_AutoComplete2: TGUID = '{EAC04BC0-3791-11d2-BB95-0060977B464C}';
 IID_ObjMgr: TGUID = '{00BB2761-6A77-11D0-A535-00C04FD7D062}';
 IID_IACList: TGUID = '{77A130B0-94FD-11D0-A544-00C04FD7d062}';
 IID_IACList2: TGUID = '{470141a0-5186-11d2-bbb6-0060977b464c}';

 CLSID_AutoComplete: TGUID = '{00BB2763-6A77-11D0-A535-00C04FD7D062}';
 CLSID_ACListISF: TGUID = '{03C036F1-A186-11D0-824A-00AA005B4383}';
 CLSID_ACLHistory: TGUID = '{00BB2764-6A77-11D0-A535-00C04FD7D062}';
 CLSID_ACLMRU: TGUID = '{6756A641-DE71-11d0-831B-00AA005B4383}';
 CLSID_ACLMulti: TGUID = '{00BB2765-6A77-11D0-A535-00C04FD7D062}';

 ACO_NONE = 0;
 ACO_AUTOSUGGEST = $01;
 ACO_AUTOAPPEND	= $02;
 ACO_SEARCH = $04;
 ACO_FILTERPREFIXES = $08;
 ACO_USETAB = $10;
 ACF_UPDOWNKEYDROPSLIST = $20;

 ACLO_NONE            = 0;    // don't enumerate anything
 ACLO_CURRENTDIR      = 1;    // enumerate current directory
 ACLO_MYCOMPUTER      = 2;    // enumerate MyComputer
 ACLO_DESKTOP         = 4;    // enumerate Desktop Folder
 ACLO_FAVORITES       = 8;    // enumerate Favorites Folder
 ACLO_FILESYSONLY     = 16;   // enumerate only the file system

type
  IObjMgr = interface(IUnknown)
  ['{00BB2761-6A77-11D0-A535-00C04FD7D062}']
    function Append(punk: IUnknown): HRESULT; stdcall;
    function Remove(punk: IUnknown): HRESULT; stdcall;
  end;

  IACList = interface(IUnknown)
  ['{77A130B0-94FD-11D0-A544-00C04FD7d062}']
    function Expand(pszExpand: PWideChar): HRESULT; stdcall;
  end;

  IACList2 = interface(IACList)
  ['{470141a0-5186-11d2-bbb6-0060977b464c}']
    function SetOptions(dwFlag: DWORD): HRESULT; stdcall;
    function GetOptions(var pdwFlag: DWORD): HRESULT; stdcall;
  end;

  IAutoComplete = interface(IUnknown)
  ['{00bb2762-6a77-11d0-a535-00c04fd7d062}']
    function Init(hwndEdit: HWND; punkACL: IUnknown; pwszRegKeyPath: PWideChar;
      pwszQuickComplete: PWideChar): HRESULT; stdcall;
    function Enable(fEnable: BOOL): HRESULT; stdcall;
  end;

  IAutoComplete2 = interface(IAutoComplete)
  ['{EAC04BC0-3791-11d2-BB95-0060977B464C}']
    function SetOptions(dwFlag: DWORD): HRESULT; stdcall;
    function GetOptions(var dwFlag: DWORD): HRESULT; stdcall;
  end;

  PPWideChar = ^PWideChar;

  IEnumString = interface(IUnknown)
  ['{00000101-0000-0000-C000-000000000046}']
    function Next(celt: ULONG; rgelt: PPWideChar;
      pceltFetched: PLongWord): HRESULT; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumString): HResult; stdcall;
  end;

  TAutoCompleteOption = (
    acoAutoSuggest,
    acoAutoAppend,
    acoSearch,
    acoFilterPrefixes,
    acoUseTab,
    acoUpDownKeyDropList
  );
  TAutoCompleteOptions = set of TAutoCompleteOption;

  TAutoCompleteSource = (
    acsAcHistory,
    acsShellNamespace,
    acsMRU,
    acsIEHistory,
    acsCustom
  );

  TEnumString = class(TInterfacedObject, IEnumString)
    FEnumPosition: Integer;
    FHistory: TStrings;
    {IEnumString}
    function Next(celt: ULONG; rgelt: PPWideChar;
      pceltFetched: PLongWord): HRESULT; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumString): HResult; stdcall;
    {IEnumString}
    constructor Create(AHistory: TStrings);
  end;

  TOnAcCreateEnumerator = procedure (var Enumerator: IEnumString) of object;

  TCustomAcEdit = class(TCustomEdit)
  private
    FACOptions: DWORD;
    FAcFailed: Boolean;
    FAutoComplete: IAutoComplete2;
    FAcEnabled: Boolean;
    FOnAcCreateEnumerator: TOnAcCreateEnumerator;
    FAcLimit: Integer;
    FAcHistory: TStrings;
    FAcSource: TAutoCompleteSource;
    procedure SetAcEnabled(const Value: Boolean);
    function GetAcOptions: TAutoCompleteOptions;
    procedure SetAcHistory(const Value: TStrings);
    procedure SetAcLimit(const Value: Integer);
    procedure SetAcOptions(const Value: TAutoCompleteOptions);
    procedure CheckAcLimit;
    procedure InitAutoComplete;
    procedure DoneAutoComplete;
    procedure SetAcSource(const Value: TAutoCompleteSource);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    function AcCreateEnumerator: IEnumString; virtual;
    property AcOptions: TAutoCompleteOptions read GetAcOptions
      write SetAcOptions;
    property AcLimit: Integer read FAcLimit write SetAcLimit;
    property AcHistory: TStrings read FAcHistory write SetAcHistory;
    property AcEnabled: Boolean read FAcEnabled write SetAcEnabled;
    property AcSource: TAutoCompleteSource read FAcSource write SetAcSource;
    property OnAcCreateEnumerator: TOnAcCreateEnumerator read FOnAcCreateEnumerator
      write FOnAcCreateEnumerator;
  public
    procedure AcAddToHistory(const S: String);
    procedure AcRemoveFromHistory(const S: String);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

  TAcEdit = class(TCustomAcEdit)
  published
    property AcEnabled;
    property AcOptions;
    property AcLimit;
    property AcHistory;
    property AcSource;
    property OnAcCreateEnumerator;
    {TEdit properties}
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses ActiveX, ComObj;

{$R *.dcr}

procedure Register;
begin
  RegisterComponents('My components', [TAcEdit]);
end;

{ TEnumString }

constructor TEnumString.Create(AHistory: TStrings);
begin
  inherited Create;
  FHistory := AHistory;
end;

function TEnumString.Reset: HResult;
begin
  FEnumPosition := 0;
  Result := S_OK;
end;

function TEnumString.Skip(celt: LongWord): HResult;
var
  Total: Integer;
begin
  Result := S_FALSE;
  Total := FHistory.Count;
  {$WARNINGS OFF}
  if FEnumPosition + celt <= Total then begin
  {$WARNINGS ON}
    Result := S_OK;
    Inc(FEnumPosition, celt)
  end;
end;

function TEnumString.Next(celt: ULONG; rgelt: PPWideChar;
  pceltFetched: PLongWord): HRESULT;
var
  I: Cardinal;
  Len: Integer;
  S: AnsiString;
begin
  Result := S_OK;
  I := 0;
  with FHistory do begin
    while (FEnumPosition < Count) and (I < celt) do begin
      S := FHistory[FEnumPosition];
      Len := MultiByteToWideChar(CP_ACP, 0, PAnsiChar(S), -1,  rgelt^, 0) *
        SizeOf(WideChar);
      rgelt^ := CoTaskMemAlloc(Len);
      MultiByteToWideChar(CP_ACP, 0, PAnsiChar(S), -1, rgelt^, Len);
      Inc(rgelt);
      Inc(I);
      Inc(FEnumPosition);
    end;
  end;
  if I <> celt then
    Result := S_FALSE;
  if Assigned(pceltFetched) then
    pceltFetched^ := I;
end;

function TEnumString.Clone(out Enum: IEnumString): HResult;
var
  EnumObject: TEnumString;
begin
  EnumObject := TEnumString.Create(FHistory);
  EnumObject.FEnumPosition := FEnumPosition;
  Enum := EnumObject as IEnumString;
  Result := S_OK;
end;

{ TCustomAcEdit }

function TCustomAcEdit.AcCreateEnumerator: IEnumString;
begin
  Result := NIL;
  case FAcSource of
  acsAcHistory: Result := TEnumString.Create(FAcHistory) as IEnumString;
  acsShellNamespace: Result := CreateComObject(CLSID_ACListISF) as IEnumString;
  acsMRU: Result := CreateComObject(CLSID_ACLMRU) as IEnumString;
  acsIEHistory: Result := CreateComObject(CLSID_ACLHistory) as IEnumString;
  acsCustom: ;
  end;
  if Assigned(FOnAcCreateEnumerator) then
    OnAcCreateEnumerator(Result);
end;

procedure TCustomAcEdit.AcAddToHistory(const S: String);
begin
  if (Length(S) > 0) and (FAcHistory.IndexOf(S) < 0) then
    FAcHistory.Insert(0, S);
end;

procedure TCustomAcEdit.CheckAcLimit;
var
  I: Integer;
begin
  if FAcLimit > 0 then begin
    for I := Pred(FAcHistory.Count) downto FAcLimit do
      FAcHistory.Delete(I);
  end;
end;

constructor TCustomAcEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAcHistory := TStringList.Create;
  FAcOptions := ACO_AUTOSUGGEST or ACO_AUTOAPPEND;
  FAcLimit := 100;
  FAcEnabled := TRUE;
  FAcSource := acsAcHistory;
end;

procedure TCustomAcEdit.CreateWnd;
begin
  inherited;
  InitAutoComplete;
end;

destructor TCustomAcEdit.Destroy;
begin
  DoneAutoComplete;
  FAcHistory.Free;
  inherited;
end;

procedure TCustomAcEdit.DestroyWnd;
begin
  inherited;
  FAutoComplete := NIL;
end;

procedure TCustomAcEdit.DoneAutoComplete;
begin
  if Assigned(FAutoComplete) then begin
    FAutoComplete := NIL;
    if Assigned(Parent) then
      Parent.Perform(WM_SETREDRAW, 0, 0);
    try
      RecreateWnd;
    finally
      if Assigned(Parent) then begin
        Parent.Perform(WM_SETREDRAW, 1, 0);
        RedrawWindow(Parent.Handle, NIL, 0, RDW_INVALIDATE);
      end;  
    end;
  end;
end;

function TCustomAcEdit.GetAcOptions: TAutoCompleteOptions;
begin
  if Assigned(FAutoComplete) then
    FAutoComplete.GetOptions(FAcOptions);
  Result := [];
  if FAcOptions and ACO_AUTOSUGGEST <> 0 then
    Include(Result, acoAutoSuggest);
  if FAcOptions and ACO_AUTOAPPEND <> 0 then
    Include(Result, acoAutoAppend);
  if FAcOptions and ACO_SEARCH <> 0 then
    Include(Result, acoSearch);
  if FAcOptions and ACO_FILTERPREFIXES <> 0 then
    Include(Result, acoFilterPrefixes);
  if FAcOptions and ACO_USETAB <> 0 then
    Include(Result, acoUseTab);
  if FAcOptions and ACF_UPDOWNKEYDROPSLIST <> 0 then
    Include(Result, acoUpDownKeyDropList);
end;

procedure TCustomAcEdit.InitAutoComplete;
var
  EnumStrings: IUnknown;
  hr: HResult;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;
  DoneAutoComplete;
  if FAcFailed or not FAcEnabled then
    Exit;
  if Handle > 0 then begin
    hr := CoCreateInstance(CLSID_AutoComplete, NIL, CLSCTX_INPROC_SERVER,
      IID_AutoComplete, FAutoComplete);
    if hr = S_OK then begin
      try
        EnumStrings := AcCreateEnumerator;
        if Assigned(EnumStrings) then begin
          OleCheck(FAutoComplete.Init(Handle, EnumStrings, NIL, NIL));
          OleCheck(FAutoComplete.SetOptions(FAcOptions));
        end else
          DoneAutoComplete;
      except
        DoneAutoComplete;
        FAcFailed := TRUE;
      end;
    end else
      FAcFailed := TRUE;
  end;
end;

procedure TCustomAcEdit.AcRemoveFromHistory(const S: String);
var
  Index: Integer;
begin
  Index := FAcHistory.IndexOf(S);
  if Index >= 0 then
    FAcHistory.Delete(Index);
end;

procedure TCustomAcEdit.SetAcEnabled(const Value: Boolean);
begin
  if FAcEnabled <> Value then begin
    FAcEnabled := Value;
    InitAutoComplete;
  end;
end;

procedure TCustomAcEdit.SetAcHistory(const Value: TStrings);
begin
  FAcHistory.Assign(Value);
end;

procedure TCustomAcEdit.SetAcLimit(const Value: Integer);
begin
  FAcLimit := Value;
  CheckAcLimit;
end;

procedure TCustomAcEdit.SetAcOptions(const Value: TAutoCompleteOptions);
begin
  FACOptions := 0;
  if acoAutoSuggest in Value then
    FAcOptions := FAcOptions or ACO_AUTOSUGGEST;
  if acoAutoAppend in Value then
    FAcOptions := FAcOptions or ACO_AUTOAPPEND;
  if acoSearch in Value then
    FAcOptions := FAcOptions or ACO_SEARCH;
  if acoFilterPrefixes in Value then
    FAcOptions := FAcOptions or ACO_FILTERPREFIXES;
  if acoUseTab in Value then
    FAcOptions := FAcOptions or ACO_USETAB;
  if acoUpDownKeyDropList in Value then
    FAcOptions := FAcOptions or ACF_UPDOWNKEYDROPSLIST;
  if Assigned(FAutoComplete) then
    FAutoComplete.SetOptions(FAcOptions);
end;

procedure TCustomAcEdit.SetAcSource(const Value: TAutoCompleteSource);
begin
  if FAcSource <> Value then begin
    FAcSource := Value;
    InitAutoComplete;
  end;
end;

end.
