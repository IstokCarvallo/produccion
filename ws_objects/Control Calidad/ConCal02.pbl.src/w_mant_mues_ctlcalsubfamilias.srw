$PBExportHeader$w_mant_mues_ctlcalsubfamilias.srw
$PBExportComments$Mantenedor de Subfamilias según sistema.
forward
global type w_mant_mues_ctlcalsubfamilias from w_mant_directo
end type
type st_1 from statictext within w_mant_mues_ctlcalsubfamilias
end type
type dw_familias from datawindow within w_mant_mues_ctlcalsubfamilias
end type
end forward

global type w_mant_mues_ctlcalsubfamilias from w_mant_directo
string tag = "Mantenedor de Tabla CTLCALSUBFAMILIA"
integer width = 2359
integer height = 2128
string title = "MAESTRO SUB FAMILIAS"
st_1 st_1
dw_familias dw_familias
end type
global w_mant_mues_ctlcalsubfamilias w_mant_mues_ctlcalsubfamilias

type variables
DataWindowChild idwc_familias
Boolean ib_agrega	
Integer ii_Grupo,ii_familia,ii_subfamilia
end variables

forward prototypes
public function boolean duplicado (integer ai_subfamilia)
public function boolean noexistefamilia (integer ai_familia)
end prototypes

public function boolean duplicado (integer ai_subfamilia);Integer 	li_Contador, li_Familia
String	al_SubFamilia

li_Familia		=	Integer(istr_mant.argumento[1])
al_SubFamilia	=	String(ai_SubFamilia)

li_Contador	= 	dw_1.Find("ccsf_codigo = " + al_SubFamilia, 1, dw_1.RowCount())

IF li_Contador = 0 OR IsNull(li_Contador) THEN
	
		SELECT	Count(*)
			INTO	:li_Contador
			FROM	dbo.ctlcalsubfamilia
			WHERE	ccfa_codigo	=	:li_Familia
			AND   ccsf_codigo =  :ai_SubFamilia;
		
		
		IF	sqlca.sqlcode	=	-1	THEN
			F_ErrorBaseDatos(sqlca,"Lectura de Tabla CTLCALSUBFAMILIA")
			RETURN TRUE
		END IF
END IF

IF li_Contador > 0 THEN
	MessageBox("Duplicidad de Datos","Sub-Familia ingresada ya existe")
	RETURN TRUE
END IF


RETURN False
	
				  
end function

public function boolean noexistefamilia (integer ai_familia);
String ls_nombre_familia

SELECT	ccfa_descrip
	INTO	:ls_nombre_familia
	FROM	dbo.CTLCALFAMILIAS
	WHERE	ccfa_codigo	=	:ai_familia AND
	      ccfa_pidesf = 1;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla CTLCALFAMILIAS")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Familia no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro código.")
	
	RETURN True
END IF	


RETURN False
end function

on w_mant_mues_ctlcalsubfamilias.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_familias=create dw_familias
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_familias
end on

on w_mant_mues_ctlcalsubfamilias.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_familias)
end on

event open;x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 1993
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)								


buscar			= "Código:Nccsf_codigo,Descripción:Sccsf_descrip"
ordenar			= "Código:ccsf_codigo,Descripción:ccsf_descrip"

istr_mant.argumento[1]	=	"0"

dw_familias.GetChild("ccfa_codigo", idwc_familias)
idwc_familias.SetTransObject(sqlca)
idwc_familias.Retrieve()
dw_familias.InsertRow(0)

pb_grabar.Enabled		=	False
pb_nuevo.Enabled		=	False
pb_lectura.Enabled	=	False
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila		 = dw_1.Retrieve(Integer(istr_mant.argumento[1]))
	
	IF ll_fila 	 = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		il_fila = 1
		//dw_1.SetRow(1)
		//dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_eliminar.enabled  = False
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalsubfamilias"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
	END IF
END IF
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF

end event

event ue_antesguardar;Long		ll_fila = 1
Integer	li_cont
String	ls_mensaje, ls_colu[]


DO WHILE ll_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
		dw_1.DeleteRow(ll_fila)
	ELSE
		ll_fila ++
	END IF
LOOP


For ll_Fila	= 1 To dw_1.RowCount()
		IF IsNull(dw_1.Object.ccsf_codigo[ll_Fila]) OR dw_1.Object.ccsf_codigo[ll_Fila] = 0 THEN
			li_cont ++
			ls_mensaje 		= ls_mensaje + "~nCódigo Sub Familia"
			ls_colu[li_cont]	= "ccsf_codigo"
		END IF
		
		IF IsNull(dw_1.Object.ccsf_descrip[ll_Fila]) OR trim(dw_1.Object.ccsf_descrip[ll_Fila]) = "" THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nDescripción"
			ls_colu[li_cont]	= "ccsf_descrip"
		END IF
		
		IF IsNull(dw_1.Object.ccsf_pidegr[ll_Fila]) OR dw_1.Object.ccsf_pidegr[ll_Fila] = 0 THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nSub Grupo"
			ls_colu[li_cont]	= "ccsf_pidegr"
		END IF

Next

	If li_cont > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	else
		Message.DoubleParm = 0
	End If
	
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ctlcalsubfamilias
integer x = 64
integer y = 24
integer width = 1801
integer height = 272
boolean enabled = true
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ctlcalsubfamilias
integer x = 1975
integer y = 316
boolean enabled = false
end type

event pb_nuevo::clicked;call super::clicked;Long ll_Null

SetNull(ll_Null)

dw_familias.Enabled		=	TRUE

pb_imprimir.Enabled		=  False
pb_grabar.Enabled			=	False
pb_eliminar.Enabled		=	False
pb_insertar.Enabled		=	False

pb_lectura.Enabled		=	FALSE

dw_familias.SetItem(1, "ccfa_codigo", ll_Null)
dw_Familias.Setfocus()
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ctlcalsubfamilias
integer x = 1975
integer y = 40
boolean enabled = false
end type

event pb_lectura::clicked;call super::clicked;
dw_familias.Enabled		=	FALSE

pb_lectura.Enabled		=	False
pb_nuevo.Enabled			=	True

end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ctlcalsubfamilias
integer x = 1975
integer y = 760
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ctlcalsubfamilias
integer x = 1975
integer y = 540
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ctlcalsubfamilias
integer x = 1984
integer y = 1580
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ctlcalsubfamilias
integer x = 1984
integer y = 1224
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ctlcalsubfamilias
integer x = 1979
integer y = 976
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ctlcalsubfamilias
integer x = 64
integer y = 360
integer width = 1801
integer height = 1428
string dataobject = "dw_mues_ctlcalsubfamilias"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event dw_1::rowfocuschanged;call super::rowfocuschanged;Integer li_Fila

li_Fila = dw_1.RowCount()

//IF li_Fila > 0 THEN
//	dw_1.SetItem(li_Fila, "ccfa_codigo", 0)
//END IF

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF

end event

event dw_1::clicked;call super::clicked;ii_subfamilia	=	dw_1.Object.ccsf_codigo[il_Fila]
end event

event dw_1::itemchanged;call super::itemchanged;Integer		li_familia
String	 	ls_Columna

ls_Columna	=	dwo.Name
li_familia	=Integer(istr_mant.argumento[1])

CHOOSE CASE ls_Columna
		
	CASE "ccsf_codigo"
		IF	Duplicado(Integer(Data))	THEN
			dw_1.DeleteRow(il_Fila)
		ELSE
			dw_1.SetItem(il_Fila, "ccfa_codigo", li_familia)
		
		END IF
END CHOOSE
end event

type st_1 from statictext within w_mant_mues_ctlcalsubfamilias
integer x = 197
integer y = 128
integer width = 256
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554431
long backcolor = 553648127
string text = "Familia"
boolean focusrectangle = false
end type

type dw_familias from datawindow within w_mant_mues_ctlcalsubfamilias
string tag = "dddw muestra familias con subfamilias"
integer x = 480
integer y = 128
integer width = 1166
integer height = 80
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_ctlcalfamilias"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Long ll_Null

SetNull(ll_Null)

IF NoexisteFamilia(integer(data)) THEN
	this.SetItem(row, "ccfa_codigo", ll_Null)
	RETURN 1  
ELSE		
	istr_mant.argumento[1]	=	data
	IF Integer(istr_mant.argumento[1])=0 OR IsNull(istr_mant.argumento[1]) THEN
		pb_lectura.Enabled	=	False	
	ELSE
		pb_lectura.Enabled	=	True
		pb_lectura.SetFocus()
	END IF
END IF	
end event

event itemerror;RETURN 1
end event

