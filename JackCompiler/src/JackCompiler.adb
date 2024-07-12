with Compilation_Engine; use Compilation_Engine;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Jack_Tokenizer;

procedure JackCompiler is
   package CLI renames Ada.Command_Line;

   JackFile : Jack_Tokenizer.File_Ptr;
   Compiler : CompilationEngine;
   Path : Unbounded_String;
   Outdirectory : Unbounded_String := To_Unbounded_String ("out_");

   procedure CompileFile (FilePath : String) is
      FileName : String := Base_Name (FilePath);
   begin
      Put_Line ("[starting compilation of " & FileName & "]");

      JackFile := new File_Type;
      Open (File => JackFile.all,
            Mode => In_File,
            Name => FilePath);

      Compiler.Initialize (InputFile     => JackFile,
                           InputFileName => FileName);
      Compiler.CompileClass;
      compiler.Close;

   Put_Line ("[compilation of " & FileName & " complete]");
   end CompileFile;

   procedure CompileDir (JackFilesPath : String) is
      JackFilePattern : String := "*.jack";
      Dir_Search      : Search_Type;
      Dir_Ent         : Directory_Entry_Type;
   begin
      Start_Search (Search    => Dir_Search,
                    Directory => JackFilesPath,
                    Pattern   => JackFilePattern);

      loop
         Get_Next_Entry (Dir_Search, Dir_Ent);

         if Kind (Dir_Ent) = Ordinary_File then
            CompileFile (Full_Name (Dir_Ent));
         end if;

         exit when not More_Entries (Dir_Search);
      end loop;
   end CompileDir;
begin
   if CLI.Argument_Count = 0 then
      Put ("please enter path: ");
      Path := Unbounded_IO.Get_Line; -- path to file
   elsif CLI.Argument_Count = 1 then
      Path := To_Unbounded_String (CLI.Argument (1));
   else
      Put_Line ("usage: JackAnalyzer.exe [file_name / folder_name]");
   end if;

   if not Exists (To_String (Path)) then
      Put_Line ("file does not exist.");
      return;
   end if;

   Path := To_Unbounded_String (Full_Name (To_String (Path)));

   if Kind (To_String (Path)) = Directory then
      Set_Directory (Directory => To_String (Path));
      Outdirectory := Outdirectory & Simple_Name (Current_Directory);
      if not Exists (Name => To_String (Outdirectory)) then
         Create_Directory (New_Directory => To_String (Outdirectory));
      end if;
      Set_Directory (Directory => To_String (Outdirectory));
      CompileDir (To_String (Path));
   else
      Set_Directory (Directory => Containing_Directory (Name => To_String (Path)));
      Outdirectory := Outdirectory & Base_Name (To_String (Path));
      if not Exists (Name => To_String (Outdirectory)) then
         Create_Directory (New_Directory => To_String (Outdirectory));
      end if;
      Set_Directory (Directory => To_String (Outdirectory));
      CompileFile (To_String (Path));
   end if;


end JackCompiler;
