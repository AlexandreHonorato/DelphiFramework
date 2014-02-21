unit _VariantEdit;

interface

uses SysUtils, Windows, Classes, StdCtrls, _Strings;

type TCheckVariantType = (cvtStartsWith, cvtContains, cvtContainsMultipleWords, cvtCustom);

type
  TCheckVariantEvent = procedure(Sender: TObject; const AText, AVariant: String; var Accept: Boolean) of object;

type
  TVarListBox = class;

  TVarEdit = class(TEdit)
  private
    FVariants: TStrings;
    FListBox: TVarListBox;
    FCanUpdateLB: Boolean;
    FAutoSizeLB: Boolean;
    FOnCheckVariant: TCheckVariantEvent;
    FCheckVariantType: TCheckVariantType;
    FOnCheckVariantsComplete: TNotifyEvent;
    procedure SetVariants(const Value: TStrings);
    procedure SetListBox(const Value: TVarListBox);
    function GetTextNoUpdate: String;
    procedure SetTextNoUpdate(const Value: String);
    procedure SetAutoSizeLB(const Value: Boolean);
    procedure AdjustLB;
    function DoCheckVariant(const AText, AVariant: String): Boolean;
    function CheckVariant_Contains(const AText, AVariant: String): Boolean;
    function CheckVariant_StartsWith(const AText, AVariant: String): Boolean;
    function CheckVariant_ContainsMultipleWords(const AText, AVariant: String): Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Text2ListBox;
  published
    property ListBox: TVarListBox read FListBox write SetListBox;
    property Variants: TStrings read FVariants write SetVariants;
    property TextNoUpdate: String read GetTextNoUpdate write SetTextNoUpdate;
    property AutoSizeLB: Boolean read FAutoSizeLB write SetAutoSizeLB default True;
    property CheckVariantType: TCheckVariantType read FCheckVariantType write FCheckVariantType default cvtContainsMultipleWords;
    property OnCheckVariant: TCheckVariantEvent read FOnCheckVariant write FOnCheckVariant;
    property OnCheckVariantsComplete: TNotifyEvent read FOnCheckVariantsComplete write FOnCheckVariantsComplete;
  end;

  TVarListBox = class(TListBox)
  private
    FEdit: TVarEdit;
    procedure Text2Edit;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetEdit(AEdit: TVarEdit);
    procedure Click; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

procedure Register;

implementation

{$R *.DCR}

procedure Register;
begin
RegisterComponents('My components', [TVarEdit, TVarListBox]);
end;

procedure TVarEdit.AdjustLB;
begin
if FAutoSizeLB then FListBox.SetBounds(Left, Top+Height, Width, FListBox.Height);
end;

procedure TVarEdit.Change;
begin
Text2ListBox;
inherited Change;
end;

constructor TVarEdit.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FCheckVariantType:=cvtContainsMultipleWords;
FAutoSizeLB:=True;
FCanUpdateLB:=True;
FVariants:=TStringList.Create;
end;

function TVarEdit.CheckVariant_Contains(const AText, AVariant: String): Boolean;
begin
Result:=Pos(AnsiUpperCase(AText), AnsiUpperCase(AVariant))<>0
end;

destructor TVarEdit.Destroy;
begin
FVariants.Free;
inherited Destroy;
end;

function TVarEdit.DoCheckVariant(const AText, AVariant: String): Boolean;
begin
case FCheckVariantType of
cvtStartsWith:
  Result:=CheckVariant_StartsWith(AText, AVariant);
cvtContains:
  Result:=CheckVariant_Contains(AText, AVariant);
cvtContainsMultipleWords:
  Result:=CheckVariant_ContainsMultipleWords(AText, AVariant);
cvtCustom:
  begin
  Result:=False;
  if Assigned(FOnCheckVariant) then
    FOnCheckVariant(Self, AText, AVariant, Result);
  end;
end; // case
end;

function TVarEdit.GetTextNoUpdate: String;
begin
Result:=Text;
end;

procedure TVarEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
if Assigned(FListBox) then
  begin
  if Key=VK_DOWN then
	  begin
	  FListBox.SetFocus;
    FListBox.ItemIndex:=0;
    FListBox.Click  
	  end;
  end;
inherited KeyDown(Key, Shift);
end;

procedure TVarEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
inherited Notification(AComponent, Operation);
if (Operation=opRemove) and (AComponent=FListBox) then FListBox:=NIL
end;

procedure TVarEdit.SetAutoSizeLB(const Value: Boolean);
begin
FAutoSizeLB:=Value;
if Assigned(FListBox) then AdjustLB;
end;

procedure TVarEdit.SetListBox(const Value: TVarListBox);
begin
if Assigned(FListBox) then FListBox.SetEdit(NIL);
FListBox:=Value;
if Assigned(FListBox) then
	begin
	FListBox.SetEdit(Self);
  AdjustLB
	end;
end;

procedure TVarEdit.SetTextNoUpdate(const Value: String);
begin
FCanUpdateLB:=False;
Text:=Value;
FCanUpdateLB:=True;
end;

procedure TVarEdit.SetVariants(const Value: TStrings);
begin
FVariants.Assign(Value);
end;

procedure TVarEdit.Text2ListBox;
var S: String; I: Integer;
begin
if (not Assigned(FListBox)) or (not FCanUpdateLB) then exit;

FListBox.Items.BeginUpdate;
FListBox.Items.Clear;

S:=Text;
if S='' then
  FListBox.Items.Assign(FVariants)
else
  for i:=0 to FVariants.Count-1 do
    if DoCheckVariant(S, FVariants[i]) then
      FListBox.Items.AddObject(FVariants[i], FVariants.Objects[i]);

FListBox.Items.EndUpdate;

if Assigned(FOnCheckVariantsComplete) then FOnCheckVariantsComplete(Self);
end;

procedure TVarListBox.Click;
begin
if Assigned(FEdit) then Text2Edit;
inherited Click;
end;

procedure TVarListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
if Assigned(FEdit) then
  begin
  if (Key=VK_UP) and ((ItemIndex<1) or (ssAlt in Shift)) then FEdit.SetFocus;
  end;
inherited KeyDown(Key, Shift);
end;

procedure TVarListBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
inherited Notification(AComponent, Operation);
if (Operation=opRemove) and (AComponent=FEdit) then SetEdit(NIL);
end;

procedure TVarListBox.SetEdit(AEdit: TVarEdit);
begin
FEdit:=AEdit;
end;

procedure TVarListBox.Text2Edit;
var I: Integer;
begin
I:=ItemIndex;
if I<>-1 then FEdit.TextNoUpdate:=Items[I];
end;

function TVarEdit.CheckVariant_StartsWith(const AText, AVariant: String): Boolean;
begin
Result:=Pos(AnsiUpperCase(AText), AnsiUpperCase(AVariant))=1
end;

function TVarEdit.CheckVariant_ContainsMultipleWords(const AText,
  AVariant: String): Boolean;
var P: Integer; S, _Word, _Variant: String;
begin
Result:=True;
S:=AText;
_Variant:=AnsiUpperCase(AVariant);
P:=Pos(';', S);
while P<>0 do
  begin
  _Word:=AnsiUpperCase(copy(S, 1, P-1));
  if _Word<>'' then Result:=Result and (Pos(_Word, _Variant)<>0);
  if not Result then break;
  S:=StrTail(S, P);
  P:=Pos(';', S);
  end;

if Result then
  begin
  _Word:=AnsiUpperCase(S);
  if _Word<>'' then Result:=Result and (Pos(_Word, _Variant)<>0);
  end; 
end;

end.