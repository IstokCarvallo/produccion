$PBExportHeader$w_mant_bitacorasitua.srw
$PBExportComments$Mantención Bitácora de Situaciones
forward
global type w_mant_bitacorasitua from window
end type
type pb_graba from picturebutton within w_mant_bitacorasitua
end type
type pb_2 from picturebutton within w_mant_bitacorasitua
end type
type dw_1 from datawindow within w_mant_bitacorasitua
end type
end forward

global type w_mant_bitacorasitua from window
integer x = 46
integer y = 48
integer width = 2930
integer height = 1644
boolean titlebar = true
string title = "Mantención Parametros de Fruta Granel"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
event ue_antesguardar pbm_custom75
event ue_guardar pbm_custom11
event ue_recuperadatos pbm_custom15
event ue_imprimir pbm_custom03
pb_graba pb_graba
pb_2 pb_2
dw_1 dw_1
end type
global w_mant_bitacorasitua w_mant_bitacorasitua

type variables
DataWindowChild   idwc_planta, idwc_tipomv

str_Mant				istr_Mant
uo_Situacion		iuo_Situacion
end variables

forward prototypes
protected function boolean wf_actualiza_db ()
end prototypes

event type long ue_antesguardar(unsignedlong wparam, long lparam);Integer	li_Contador
String	ls_Mensaje, ls_Columna[]

IF IsNull(dw_1.Object.situ_codigo[1]) OR &
	dw_1.Object.situ_codigo[1] = 0 THEN
	li_Contador ++
	ls_Mensaje 					+=	"~nCódigo de Situación"
	ls_Columna[li_Contador]	=	"situ_codigo"
END IF

IF iuo_Situacion.Observacion = 1 THEN
	IF IsNull(dw_1.Object.bita_observ[1]) OR &
		dw_1.Object.bita_observ[1] = "" THEN
		li_Contador ++
		ls_Mensaje 					+=	"~nObservaciones"
		ls_Columna[li_Contador]	=	"bita_observ"
	END IF
END IF

IF li_Contador > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_Mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_Columna[1])
	dw_1.SetFocus()
	Message.DoubleParm = -1
END IF

RETURN 0
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

protected function boolean wf_actualiza_db ();Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_1.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_1.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_mant_bitacorasitua.create
this.pb_graba=create pb_graba
this.pb_2=create pb_2
this.dw_1=create dw_1
this.Control[]={this.pb_graba,&
this.pb_2,&
this.dw_1}
end on

on w_mant_bitacorasitua.destroy
destroy(this.pb_graba)
destroy(this.pb_2)
destroy(this.dw_1)
end on

event open;/*
	Argumentos Traspasados
		[1]	=	Sistema	:		1	=	Fruta Granel
									2	=	Fruta Embalada
									3	=	Fruta Comercial
									4	=	Control de Envases
		[2]	=	Planta
		[3]	=	Identificador
		[4]	=	Tipo de Acción	:	I	=	Ingreso
											M	=	Modificación
											E	=	Eliminación
		[5]	=	Planta del Identificador
		[6]	=	Especie o Código de Exportador (Según Módulo 1 = Especie / 2 = Exportador)
		[7]	=	Correlativo del Lote o Número de Pallet (Según Módulo 1 = Lote / 2 = Pallet)
*/

Long			ll_fila
DateTime	ldt_fecha

istr_Mant		=	Message.PowerObjectParm
x					=	0
y					=	0
iuo_Situacion	=	CREATE uo_Situacion

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")

dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(Sqlca)

IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención", "Falta Registrar Plantas")
	idwc_planta.InsertRow(0)
END IF

dw_1.GetChild("situ_codigo", idwc_tipomv)
idwc_tipomv.SetTransObject(Sqlca)

IF idwc_tipomv.Retrieve(istr_Mant.Argumento[4]) = 0 THEN
	MessageBox("Atención", "Falta Registrar Tipos de Situaciones")
	idwc_tipomv.InsertRow(0)
END IF

ldt_Fecha	=	F_FechaHora()

dw_1.InsertRow(0)

dw_1.Object.bita_modulo[1]	=	Integer(istr_Mant.Argumento[1])
dw_1.Object.plde_codigo[1]	=	Integer(istr_Mant.Argumento[2])
dw_1.Object.bita_identi[1]	=	istr_Mant.Argumento[3]
dw_1.Object.situ_tipacc[1]	=	istr_Mant.Argumento[4]
dw_1.Object.bita_feceve[1]	=	DateTime(Date(ldt_Fecha))
dw_1.Object.bita_horeve[1]	=	Time(String(ldt_Fecha, 'hh:mm:ss'))

IF UpperBound(istr_Mant.Argumento) > 6 THEN
	
	IF istr_Mant.Argumento[1] = '1' THEN
		dw_1.Object.lote_pltcod[1]	=	Integer(istr_Mant.Argumento[5])
		dw_1.Object.lote_espcod[1]	=	Integer(istr_Mant.Argumento[6])
		dw_1.Object.lote_codigo[1]	=	Integer(istr_Mant.Argumento[7])
	ELSE
		dw_1.Object.lote_pltcod[1]	=	Integer(istr_Mant.Argumento[5])
		dw_1.Object.expo_codigo[1]	=	Integer(istr_Mant.Argumento[6])
		dw_1.Object.paen_numero[1]	=	Long(istr_Mant.Argumento[7])
	END IF	
	
END IF

dw_1.Object.bita_usuari[1]	=	gstr_Us.Nombre
end event

event resize;This.Height			= dw_1.Height + 232
This.Width			= dw_1.width + 540

dw_1.x				= 78
dw_1.y				= 72

pb_graba.x			= dw_1.x + dw_1.Width + 70
pb_graba.y			= dw_1.y
end event

event closequery;IF dw_1.RowCount() > 0 THEN
	CHOOSE CASE MessageBox("Grabar registro(s)","Desea Grabar la información ?", Question!, YesNoCancel!)
		CASE 1
			Message.DoubleParm = 0
			This.triggerevent("ue_guardar")
			IF message.doubleparm = -1 THEN Message.ReturnValue = 1
			RETURN
		CASE 3
			Message.ReturnValue = 1
			RETURN
	END CHOOSE
END IF
end event

type pb_graba from picturebutton within w_mant_bitacorasitua
event ue_mousemove pbm_mousemove
integer x = 2496
integer y = 420
integer width = 302
integer height = 244
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Todo.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Todo-BN.png"
alignment htextalign = left!
end type

event ue_mousemove;w_main.SetMicroHelp("Grabar actual información")
end event

event clicked;Parent.TriggerEvent("ue_guardar")

Close(Parent)
end event

type pb_2 from picturebutton within w_mant_bitacorasitua
event ue_mousemove pbm_mousemove
boolean visible = false
integer x = 2496
integer y = 772
integer width = 302
integer height = 244
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apaga-BNr.png "
alignment htextalign = left!
end type

event ue_mousemove;w_main.SetMicroHelp("Salir de Parametros")
end event

event clicked;Close(Parent)
end event

type dw_1 from datawindow within w_mant_bitacorasitua
integer x = 64
integer y = 64
integer width = 2359
integer height = 1460
integer taborder = 10
string dataobject = "dw_mant_bitacorasitua"
boolean border = false
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	ls_Columna, ls_Nula

SetNull(ls_Nula)

ls_Columna	=	dwo.Name

CHOOSE CASE ls_Columna
	CASE "situ_codigo"
		IF Not iuo_Situacion.Existe(istr_Mant.Argumento[4], &
			Integer(Data), True, SQLCA) THEN
			This.SetItem(1, ls_Columna, ls_Nula)
			
			RETURN 1
		ELSEIF iuo_Situacion.Modulo <> Integer(istr_Mant.Argumento[1]) THEN
			MessageBox("Atención", "Situación no Corresponde a Sistema")
			
			RETURN 1
		ELSE
			IF iuo_Situacion.Observacion = 1 THEN
				This.Object.bita_observ.Protect				=	0
				This.Object.bita_observ.BackGround.Color	=	RGB(255, 255, 255)
			ELSE
				This.Object.bita_observ.Protect				=	1
				This.Object.bita_observ.BackGround.Color	=	RGB(192, 192, 192)
			END IF
		END IF

END CHOOSE


end event

event itemerror;Return 1
end event

