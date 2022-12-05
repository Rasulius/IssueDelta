unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    CreateSearchFormButton: TButton;
    LoadDocumentButton: TButton;
    SearchFormsList: TListBox;
    OpenDialog: TOpenDialog;

    procedure CreateSearchFormButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadDocumentButtonClick(Sender: TObject);
  private
    FText: TStrings;
    procedure PrintAllSearchForms;
  public
    constructor Create(anOwner: TComponent); override;
    destructor Destroy; override;

  end;

var
  MainFormS: TMainForm;

implementation

uses SearchForm;

{$R *.lfm}

{ TMainForm }


constructor TMainForm.Create(anOwner: TComponent);
begin
  inherited Create(anOwner);
  FText := TStrings.Create;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FText);
  inherited Destroy;
end;

procedure TMainForm.LoadDocumentButtonClick(Sender: TObject);
begin
 if OpenDialog.Execute then begin
   FText.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TMainForm.PrintAllSearchForms;
var
   i: Integer;
begin
  SearchFormsList.Clear;

  for i := 0 to Application.ComponentCount - 1 do begin
    if Application.Components[i] is TSearchForm then begin
      SearchFormsList.Items.Add((Application.Components[i] as TSearchForm).Name);
    end;
  end;

end;


procedure TMainForm.CreateSearchFormButtonClick(Sender: TObject);
begin
  ShowSearchForm(Owner, FText);
  PrintAllSearchForms;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //
end;

end.

