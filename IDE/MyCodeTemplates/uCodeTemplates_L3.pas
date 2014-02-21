unit uCodeTemplates_L3;

interface

//UAL 3

uses SysUtils, Classes, uCodeTemplates_L4, uCodeTemplates;

type TTemplateVarEx = class
  private
    FName: String;
    FValue: String;
    FDefaultValue: String;
    FCaption: String;
  public
    property Name: String read FName;
    property Value: String read FValue write FValue;
    property DefaultValue: String read FDefaultValue;
    property Caption: String read FCaption;
  end;                  

type TTemplateVarsEx = class(TList)
  private
    function GetVars(Index: Integer): TTemplateVarEx;
    function GetByName(const AName: String): TTemplateVarEx;
  public
    property ByName[const AName: String]: TTemplateVarEx read GetByName;
    procedure Clear; override;
    property Vars[Index: Integer]: TTemplateVarEx read GetVars; default;
    function NewVar(const ACaption, AName, AValue, ADefaultValue: String): TTemplateVarEx;
  end;

type TTemplateBlock = class(TCustomTemplateProcessor)
  private
    FVars: TTemplateVarsEx;
    FCaption: String;
  protected
    function CompileToken(const AToken: String): String; override;
  public
    property Caption: String read FCaption write FCaption;
    property Vars: TTemplateVarsEx read FVars write FVars;
    procedure Reset(AFullReset: Boolean); override;
  end;

type TCTFolder = class;

  TCTItem = class
  private
    FCaption: String;
    FParent: TCTFolder;
    FHotKey: Char;
  public
    constructor Create(AParent: TCTFolder); virtual;
    property Parent: TCTFolder read FParent;
    property Caption: String read FCaption write FCaption;
    property HotKey: Char read FHotKey write FHotKey;
  end;

  TCTTemplate = class(TCTItem)
  private
    FVars: TTemplateVarsEx;
    FBlocks: TList;
    FShortCut: String;
    function GetBlocks(Index: Integer): TTemplateBlock;
    function GetBlockCount: Integer;
    procedure SetBlockCount(const Value: Integer);
  public
    function VarsDefined: Boolean;
    procedure ResetVars(AParams: TStrings);
    property Vars: TTemplateVarsEx read FVars write FVars;
    constructor Create(AParent: TCTFolder); override;
    destructor Destroy; override;
    property BlockCount: Integer read GetBlockCount write SetBlockCount;
    property Blocks[Index: Integer]: TTemplateBlock read GetBlocks;
    property ShortCut: String read FShortCut write FShortCut;
  end;

  TCTFolder = class(TCTItem)
  private
    FItems: TList;
    function GetItems(Index: Integer): TCTItem;
  public
    constructor Create(AParent: TCTFolder); override;
    destructor Destroy; override;
    procedure Clear;
    property Items[Index: Integer]: TCTItem read GetItems; default;
    function Count: Integer;
    function NewTemplate: TCTTemplate;
    function NewFolder: TCTFolder;
  end;

type TCT_BLL = class(TCTFolder)
  private
    FCurrTemplate: TCTTemplate;
    FShortCuts: TStringList;
  public
    constructor Create(AParent: TCTFolder); override;
    destructor Destroy; override;
    property CurrTemplate: TCTTemplate read FCurrTemplate write FCurrTemplate;
    property ShortCuts: TStringList read FShortCuts;
  end;

var CT_BLL: TCT_BLL;

procedure Init_CT_BLL;
procedure Done_CT_BLL;

implementation

uses Math;

procedure Init_CT_BLL;
begin
Init_CT_PL;
CT_BLL:=TCT_BLL.Create(NIL);
CT_PL.ReadTemplates(CT_BLL);
end;

procedure Done_CT_BLL;
begin
CT_BLL.Free;
Done_CT_PL;
end;

{ TCTTemplate }

constructor TCTTemplate.Create(AParent: TCTFolder);
begin
inherited Create(AParent);
FVars:=TTemplateVarsEx.Create;
FBlocks:=TList.Create;
end;

destructor TCTTemplate.Destroy;
var I: Integer;
begin
for i:=0 to FBlocks.Count-1 do Blocks[i].Free;
FBlocks.Free;
FVars.Free;
inherited Destroy;
end;

function TCTTemplate.GetBlockCount: Integer;
begin
Result:=FBlocks.Count;
end;

function TCTTemplate.GetBlocks(Index: Integer): TTemplateBlock;
begin
Result:=TTemplateBlock(FBlocks[Index])
end;

procedure TCTTemplate.ResetVars(AParams: TStrings);
var I, Cnt: Integer;
begin
for i:=0 to FVars.Count-1 do
  with FVars[i] do
    Value:=DefaultValue;
if (AParams<>NIL) then // проверки FVars.Count<>0 и AParams.Count<>0 не нужны, т.к. если кто-то из них =0, то Cnt будет =0 и цикл выполняться не будет
	begin
  Cnt:=Min(FVars.Count, AParams.Count);
  for i:=0 to Cnt-1 do FVars[i].Value:=AParams[i];
	end;
end;

procedure TCTTemplate.SetBlockCount(const Value: Integer);
var I: Integer; B: TTemplateBlock;
begin
for i:=0 to FBlocks.Count-1 do Blocks[i].Free;
for i:=0 to Value-1 do
	begin
  B:=TTemplateBlock.Create;
  B.Vars:=FVars;
	FBlocks.Add(B);
	end;
end;

function TCTTemplate.VarsDefined: Boolean;
var I: Integer;
begin
Result:=True;
for i:=0 to FVars.Count-1 do
  if FVars[i].Value='' then
    begin
    Result:=False;
    break;
    end;
end;

{ TTemplateVarsEx }

procedure TTemplateVarsEx.Clear;
var I: Integer;
begin
for i:=0 to Count-1 do Vars[i].Free;
inherited Clear;
end;

function TTemplateVarsEx.GetByName(const AName: String): TTemplateVarEx;
var I: Integer; Tmp: TTemplateVarEx;
begin
Result:=NIL;
for i:=0 to Count-1 do
  begin
  Tmp:=Vars[i];
  if Tmp.Name=AName then
    begin
    Result:=Tmp;
    break;
    end;
  end;
end;

function TTemplateVarsEx.GetVars(Index: Integer): TTemplateVarEx;
begin
Result:=TTemplateVarEx(Items[Index]);
end;

function TTemplateVarsEx.NewVar(const ACaption, AName, AValue, ADefaultValue: String): TTemplateVarEx;
begin
Result:=TTemplateVarEx.Create;
Result.FCaption:=ACaption;
Result.FName:=AName;
Result.FValue:=AValue;
Result.FDefaultValue:=ADefaultValue;
Add(Result)
end;

{ TTemplateProcessorEx }

function TTemplateBlock.CompileToken(const AToken: String): String; 
begin
Result:=FVars.ByName[AToken].Value;
end;

{ TCTItem }

constructor TCTItem.Create(AParent: TCTFolder);
begin
inherited Create;
FParent:=AParent;
end;

{ TCTFolder }

procedure TCTFolder.Clear;
var I: Integer;
begin
for i:=0 to FItems.Count-1 do Items[I].Free;
FItems.Clear;
end;

function TCTFolder.Count: Integer;
begin
Result:=FItems.Count;
end;

constructor TCTFolder.Create(AParent: TCTFolder);
begin
inherited Create(AParent);
FItems:=TList.Create;
end;

destructor TCTFolder.Destroy;
begin
Clear;
FItems.Free;
inherited Destroy;
end;

function TCTFolder.GetItems(Index: Integer): TCTItem;
begin
Result:=TCTItem(FItems[Index])
end;

function TCTFolder.NewFolder: TCTFolder;
begin
Result:=TCTFolder.Create(Self);
FItems.Add(Result);
end;

function TCTFolder.NewTemplate: TCTTemplate;
begin
Result:=TCTTemplate.Create(Self);
FItems.Add(Result);
end;

{ TCT_BLL }

constructor TCT_BLL.Create(AParent: TCTFolder);
begin
inherited Create(AParent);
FShortCuts:=TStringList.Create;
FShortCuts.Sorted:=True;
end;

destructor TCT_BLL.Destroy;
begin
FShortCuts.Free;
inherited Destroy;
end;

procedure TTemplateBlock.Reset(AFullReset: Boolean);
begin
inherited Reset(AFullReset);
if AFullReset then FVars.Clear;
end;

end.
