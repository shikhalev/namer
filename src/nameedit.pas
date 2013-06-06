unit NameEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, LazIDEIntf, PropEdits, LCLType;

type
  TfrmNameEdit = class(TForm)
    btnAuto : TBitBtn;
    btnDelete : TBitBtn;
    btnEdit : TBitBtn;
    btnCancel : TBitBtn;
    btnOK : TBitBtn;
    btnSave : TBitBtn;
    btnCancelPrefix : TBitBtn;
    cbxClass : TComboBox;
    editPrefix : TEdit;
    editName : TEdit;
    rgScope : TRadioGroup;
    procedure btnAutoClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure btnCancelPrefixClick(Sender : TObject);
    procedure btnDeleteClick(Sender : TObject);
    procedure btnEditClick(Sender : TObject);
    procedure btnOKClick(Sender : TObject);
    procedure btnSaveClick(Sender : TObject);
    procedure cbxClassChange(Sender : TObject);
    procedure editNameKeyPress(Sender : TObject; var Key : char);
    procedure editPrefixKeyPress(Sender : TObject; var Key : char);
    procedure FormActivate(Sender : TObject);
    procedure FormCreate(Sender : TObject);
  private
    fldSysPrefs : TStrings;
    fldUsrPrefs : TStrings;
    fldPrjPrefs : TStrings;
    class function SysPrefsFile : String;
    class function UsrPrefsFile : String;
    class function PrjPrefsFile : String;
    procedure editNameMode;
    procedure editPrefixMode;
    procedure fillClasses;
    procedure loadPrefixes;
  public
    Edited : TComponent;
    ResultName : String;
    ResultCaption : String;
    OldCaption : String;
    procedure ParseName;
    procedure FindPrefix;
    function classPrefix (const cls : String) : String;
    class function Execute (cmp : TComponent) : Boolean;
    class function Generate (const cls : String) : String;
  end;

type
  TNameEdit = class(TComponentNamePropertyEditor)
    function GetAttributes : TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

{$R *.lfm}

procedure Register;
 begin
  RegisterPropertyEditor(TypeInfo(AnsiString), TComponent, 'Name', TNameEdit);
 end;

const
  PrefsFileName = 'nameedit.lst';

{ TfrmNameEdit }

procedure TfrmNameEdit.btnEditClick(Sender : TObject);
 begin
  fillClasses;
  editPrefixMode;
  editPrefix.Text := classPrefix(cbxClass.Text);
 end;

procedure TfrmNameEdit.btnOKClick(Sender : TObject);
 begin
  ResultName := editPrefix.Text + editName.Text;
  ResultCaption := editName.Text;
  self.ModalResult := mrOK;
 end;

procedure TfrmNameEdit.btnSaveClick(Sender : TObject);
 begin
  if rgScope.ItemIndex = 0
     then begin
           fldUsrPrefs.Values[cbxClass.Text] := editPrefix.Text;
           fldUsrPrefs.SaveToFile(UsrPrefsFile);
          end
     else begin
           fldPrjPrefs.Values[cbxClass.Text] := editPrefix.Text;
           fldPrjPrefs.SaveToFile(PrjPrefsFile);
          end;
  editNameMode;
 end;

procedure TfrmNameEdit.cbxClassChange(Sender : TObject);
 begin
  editPrefix.Text := classPrefix(cbxClass.Text);
 end;

procedure TfrmNameEdit.editNameKeyPress(Sender : TObject; var Key : char);
 begin
  if Key = #13
     then btnOK.Click;
  if not (UpCase(Key) in ['A'..'Z', '0'..'9', '_', #8, #127])
     then Key := #0;
 end;

procedure TfrmNameEdit.editPrefixKeyPress(Sender : TObject; var Key : char);
 begin
  if Key = #13
     then btnSave.Click;
  if not (UpCase(Key) in ['A'..'Z', '0'..'9', '_', #8, #127])
     then Key := #0;
 end;

procedure TfrmNameEdit.FormActivate(Sender : TObject);
 begin
  if editName.Visible
     then editName.SetFocus;
 end;

procedure TfrmNameEdit.btnAutoClick(Sender : TObject);
 begin
  editPrefix.Text := Generate(cbxClass.Text);
 end;

procedure TfrmNameEdit.btnCancelClick(Sender : TObject);
 begin
  self.ModalResult := mrCancel;
 end;

procedure TfrmNameEdit.btnCancelPrefixClick(Sender : TObject);
 begin
  editNameMode;
 end;

procedure TfrmNameEdit.btnDeleteClick(Sender : TObject);
 var
   i : Integer;
 begin
  i := fldPrjPrefs.IndexOfName(cbxClass.Text);
  if i <> -1
     then begin
           fldPrjPrefs.Delete(i);
           fldPrjPrefs.SaveToFile(PrjPrefsFile);
          end;
  i := fldUsrPrefs.IndexOfName(cbxClass.Text);
  if i <> -1
     then begin
           fldUsrPrefs.Delete(i);
           fldUsrPrefs.SaveToFile(UsrPrefsFile);
          end;
 end;

procedure TfrmNameEdit.FormCreate(Sender : TObject);
 begin
  fldSysPrefs := TStringList.Create;
  fldUsrPrefs := TStringList.Create;
  fldPrjPrefs := TStringList.Create;
  loadPrefixes;
 end;

class function TfrmNameEdit.SysPrefsFile : String;
 begin
  result := LazarusIDE.GetSecondaryConfigPath + '/' + PrefsFileName;
 end;

class function TfrmNameEdit.UsrPrefsFile : String;
 begin
  result := LazarusIDE.GetPrimaryConfigPath + '/' + PrefsFileName;
 end;

class function TfrmNameEdit.PrjPrefsFile : String;
 begin
  result := ExtractFilePath(LazarusIDE.ActiveProject.MainFile.Filename) + '.' + PrefsFileName;
 end;

procedure TfrmNameEdit.editNameMode;
 begin
  editPrefix.Enabled := false;
  self.FindPrefix;
  editName.Visible := true;
  btnEdit.Visible := true;
  btnCancel.Visible := true;
  btnOK.Visible := true;
  btnSave.Visible := false;
  btnCancelPrefix.Visible := false;
  cbxClass.Visible := false;
  btnDelete.Visible := false;
  btnAuto.Visible := false;
  rgScope.Visible := false;
 end;

procedure TfrmNameEdit.editPrefixMode;
 begin
  editPrefix.Enabled := true;
  editName.Visible := false;
  btnEdit.Visible := false;
  btnCancel.Visible := false;
  btnOK.Visible := false;
  btnSave.Visible := true;
  btnCancelPrefix.Visible := true;
  cbxClass.Visible := true;
  cbxClass.ItemIndex := 0;
  btnDelete.Visible := true;
  btnAuto.Visible := true;
  rgScope.Visible := true;
 end;

procedure TfrmNameEdit.fillClasses;
 var
   cls : TClass;
 begin
  cls := Edited.ClassType;
  cbxClass.Clear;
  if not ((Edited is TForm) or (Edited is TDataModule) or (Edited is TFrame))
     then cbxClass.AddItem(cls.ClassName, nil);
  repeat
    cls := cls.ClassParent;
    cbxClass.AddItem(cls.ClassName, nil);
  until (cls = TComponent) or (cls = TObject);
 end;

procedure TfrmNameEdit.loadPrefixes;
 begin
  fldSysPrefs.Clear;
  if FileExists(SysPrefsFile)
     then fldSysPrefs.LoadFromFile(SysPrefsFile);
  fldUsrPrefs.Clear;
  if FileExists(UsrPrefsFile)
     then fldUsrPrefs.LoadFromFile(UsrPrefsFile);
  fldPrjPrefs.Clear;
  if FileExists(PrjPrefsFile)
     then fldPrjPrefs.LoadFromFile(PrjPrefsFile);
 end;

procedure TfrmNameEdit.ParseName;
 var
   i : Integer;
 begin
  for i := 1 to Length(Edited.Name) do
      if not (Edited.Name[i] in ['a'..'z', '0'..'9', '_'])
         then begin
               OldCaption := Copy(Edited.Name, i, Length(Edited.Name) - i + 1);
               editName.Text := OldCaption;
               Exit;
              end;
  OldCaption := Edited.Name;
  editName.Text := OldCaption;
 end;

procedure TfrmNameEdit.FindPrefix;
 var
   cls : TClass;
   pfx : String;
 begin
  cls := Edited.ClassType;
  repeat
    pfx := classPrefix(cls.ClassName);
    if pfx <> ''
       then begin
             editPrefix.Text := pfx;
             Exit;
            end;
    cls := cls.ClassParent;
  until (cls = TComponent) or (cls = TObject);
  editPrefix.Text := 'cmp';
 end;

function TfrmNameEdit.classPrefix(const cls : String) : String;
 begin
  result := fldPrjPrefs.Values[cls];
  if result = ''
     then result := fldUsrPrefs.Values[cls];
  if result = ''
     then result := fldSysPrefs.Values[cls];
 end;

var
  frmNameEdit : TfrmNameEdit = nil;

class function TfrmNameEdit.Execute(cmp : TComponent) : Boolean;
 begin
  if frmNameEdit = nil
     then frmNameEdit := TfrmNameEdit.Create(nil);
  frmNameEdit.Edited := cmp;
  frmNameEdit.ParseName;
  frmNameEdit.FindPrefix;
  if frmNameEdit.ShowModal = mrOK
     then begin
           cmp.Name := frmNameEdit.ResultName;
           if (cmp is TControl) and (TControl(cmp).Caption = frmNameEdit.OldCaption)
              then TControl(cmp).Caption := frmNameEdit.ResultCaption;
           result := true;
          end
     else result := false;
 end;

class function TfrmNameEdit.Generate(const cls : String) : String;

 var
   nm : String;

 function checkShort (out px : String) : Boolean;
  begin
   if Length(nm) <= 4
      then begin
            px := LowerCase(nm);
            result := true;
           end
      else result := false;
  end;

 function checkAbbr (out px : String) : Boolean;
  var
    a : String;
    i : Integer;
  begin
   a := '';
   for i := 0 to Length(nm) do
       if nm[i] in ['A'..'Z', '0'..'9']
          then a := a + nm[i];
   if Length(a) >= 2
      then begin
            px := LowerCase(a);
            result := true;
           end
      else result := false;
  end;

 function vowel : String;
  var
    i : Integer;
    a : String;
  begin
   a := '';
   i := 1;
   while UpCase(nm[i]) in ['A', 'E', 'I', 'O', 'U', 'Y'] do
         begin
          a := a + nm[i];
          i := i + 1;
         end;
   if Length(a) >= 3
      then begin
            result := LowerCase(a);
            Exit;
           end;
   while not (UpCase(nm[i]) in ['A', 'E', 'I', 'O', 'U', 'Y']) do
         begin
          a := a + nm[i];
          i := i + 1;
         end;
   result := LowerCase(a);
  end;

 function consonant : String;
  var
    i : Integer;
    a : String;
  begin
   a := nm[1];
   for i := 2 to Length(nm) do
       if (nm[i] <> nm[i - 1]) and not (UpCase(nm[i]) in ['A', 'E', 'I', 'O', 'U', 'Y'])
          then a := a + nm[i];
   result := LowerCase(Copy(a, 1, 3));
  end;

 begin
  if cls[1] = 'T'
     then nm := Copy(cls, 2, Length(cls) - 1)
     else nm := cls;
  if checkShort(result) or checkAbbr(result)
     then Exit;
  if UpCase(nm[1]) in ['A', 'E', 'I', 'O', 'U', 'Y']
     then result := vowel
     else result := consonant
 end;

{ TNameEdit }

function TNameEdit.GetAttributes : TPropertyAttributes;
 begin
  result := [paDialog];
 end;

procedure TNameEdit.Edit;
 begin
  TfrmNameEdit.Execute(TComponent(GetComponent(0)));
 end;

end.

