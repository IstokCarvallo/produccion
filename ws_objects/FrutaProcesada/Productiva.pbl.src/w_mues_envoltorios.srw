$PBExportHeader$w_mues_envoltorios.srw
forward
global type w_mues_envoltorios from w_mant_directo
end type
end forward

global type w_mues_envoltorios from w_mant_directo
integer width = 3232
string title = "ENVOLTORIO"
end type
global w_mues_envoltorios w_mues_envoltorios

type variables
uo_grabatablas		iuo_grabatablas
end variables

forward prototypes
public function boolean duplicado (string codigo)
public subroutine wf_replicacion ()
end prototypes

public function boolean duplicado (string codigo);Long		ll_fila
String	ls_codigo
	
	ll_fila	= dw_1.Find("envo_codigo = " +  codigo , 1, dw_1.RowCount())	
	
	IF ll_fila > 0 and ll_fila <> il_fila THEN
		MessageBox("Error","Código de Envoltorio, ya fue ingresado anteriormente",Information!, Ok!)
		RETURN True
	ELSE
		RETURN False
	END IF

		
end function

public subroutine wf_replicacion ();Integer	ll_fila
ll_fila = dw_1.GetNextModified(ll_fila, Primary!)
IF ll_fila > 0 THEN
	IF iuo_grabatablas.existereplicatablas(gi_CodExport) AND gstr_apl.CodigoSistema = 23 THEN
		iuo_grabatablas.replicatabla_envoltorios(dw_1)
	END IF
END IF	
end subroutine

on w_mues_envoltorios.create
call super::create
end on

on w_mues_envoltorios.destroy
call super::destroy
end on

event open;call super::open;ordenar = "Código Envoltorio:Nenvo_codigo,Descripción:Senvo_descrip"
buscar  = "Código Envoltorio:envo_codigo,Descripción:envo_descrip"

iuo_grabatablas		=	CREATE uo_grabatablas

IF gstr_apl.CodigoSistema <> 23 THEN
	IF gi_codexport = gi_cliebase THEN
		istr_mant.Solo_Consulta = True
	END IF	
END IF	
end event

event ue_antesguardar();call super::ue_antesguardar;FOR il_fila = 1 TO dw_1.RowCount()
	IF dw_1.GetItemStatus(il_fila, 0, Primary!) = New! OR dw_1.GetItemStatus(il_fila, 0, Primary!) = NewModified! THEN
		TriggerEvent("ue_validaregistro")
		
	END IF
NEXT
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN Return

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO ENVOLTORIOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_envoltorios"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE	
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_nuevo();call super::ue_nuevo;
dw_1.SetColumn("envo_codigo")
end event

event ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve()
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		IF istr_mant.Solo_Consulta <> True THEN
			pb_insertar.Enabled	= True
			pb_eliminar.Enabled	= True
			pb_grabar.Enabled		= True
		END IF	
		pb_imprimir.Enabled	= True
		il_fila					= 1
	ELSE
		IF istr_mant.Solo_Consulta <> True THEN
			pb_insertar.Enabled	= True
			pb_insertar.SetFocus()
		END IF	
		ias_campo[1]			= ""
		ias_campo[2]			= ""
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)

end event

event ue_validaregistro();call super::ue_validaregistro;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.envo_codigo[il_fila]) OR dw_1.Object.envo_codigo[il_fila] = 0 THEN
	li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Envoltorio"
	ls_colu[li_cont]	= "envo_codigo"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF
end event

type st_encabe from w_mant_directo`st_encabe within w_mues_envoltorios
boolean visible = false
integer x = 0
integer y = 1568
integer width = 1586
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mues_envoltorios
integer x = 2807
integer y = 404
end type

event pb_nuevo::clicked;IF Not istr_mant.Solo_Consulta THEN
	CHOOSE CASE wf_modifica()
		CASE 0
			CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
				CASE 1
					Message.DoubleParm = 0
					Parent.TriggerEvent("ue_guardar")
					IF message.DoubleParm = -1 THEN RETURN
					
				CASE 3
					RETURN
			END CHOOSE
	END CHOOSE
END IF

wf_BloqueaColumnas(False)

dw_1.Reset()

istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta

IF gstr_apl.CodigoSistema <> 23 THEN
	IF gi_codexport = gi_cliebase THEN
//		IF gi_codplanta <> 2 AND gi_codplanta <> 3 AND gi_codplanta <> 4 THEN
			istr_mant.Solo_Consulta = True
//		END IF	
	END IF	
END IF	
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mues_envoltorios
integer x = 2807
integer y = 108
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mues_envoltorios
integer x = 2807
integer y = 764
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mues_envoltorios
integer x = 2807
integer y = 584
end type

type pb_salir from w_mant_directo`pb_salir within w_mues_envoltorios
integer x = 2807
integer y = 1508
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mues_envoltorios
integer x = 2807
integer y = 1124
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mues_envoltorios
integer x = 2807
integer y = 944
end type

type dw_1 from w_mant_directo`dw_1 within w_mues_envoltorios
integer y = 104
integer width = 2789
integer height = 1396
string dataobject = "dw_mues_envoltorios"
end type

event dw_1::itemchanged;call super::itemchanged;String	ls_null

SetNull(ls_null)
CHOOSE CASE GetColumnName()
			
	CASE "envo_codigo"
		
		IF Duplicado(data) THEN
			This.SetItem(il_fila, "envo_codigo",Long(ls_null))
			RETURN 1
		END IF
		
END CHOOSE
end event

