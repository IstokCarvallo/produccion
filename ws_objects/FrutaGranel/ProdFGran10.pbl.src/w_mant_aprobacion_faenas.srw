$PBExportHeader$w_mant_aprobacion_faenas.srw
$PBExportComments$Mantenedor de Agrónomos.
forward
global type w_mant_aprobacion_faenas from w_mant_tabla
end type
type st_1 from statictext within w_mant_aprobacion_faenas
end type
type st_4 from statictext within w_mant_aprobacion_faenas
end type
type dw_entidad from datawindow within w_mant_aprobacion_faenas
end type
type em_fecha from editmask within w_mant_aprobacion_faenas
end type
type cbx_todos from checkbox within w_mant_aprobacion_faenas
end type
type dw_remu from uo_dw within w_mant_aprobacion_faenas
end type
type st_2 from statictext within w_mant_aprobacion_faenas
end type
type dw_conexi from datawindow within w_mant_aprobacion_faenas
end type
type sle_rut from singlelineedit within w_mant_aprobacion_faenas
end type
type cb_buscar from commandbutton within w_mant_aprobacion_faenas
end type
type sle_nombre from singlelineedit within w_mant_aprobacion_faenas
end type
type dw_fechas from uo_dw within w_mant_aprobacion_faenas
end type
type st_3 from statictext within w_mant_aprobacion_faenas
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_aprobacion_faenas
end type
type dw_persona from uo_dw within w_mant_aprobacion_faenas
end type
type cb_1 from commandbutton within w_mant_aprobacion_faenas
end type
type cb_2 from commandbutton within w_mant_aprobacion_faenas
end type
type cb_3 from commandbutton within w_mant_aprobacion_faenas
end type
type cb_4 from commandbutton within w_mant_aprobacion_faenas
end type
type st_7 from statictext within w_mant_aprobacion_faenas
end type
type st_6 from statictext within w_mant_aprobacion_faenas
end type
type sle_mensa from singlelineedit within w_mant_aprobacion_faenas
end type
type dw_destinatarios from datawindow within w_mant_aprobacion_faenas
end type
type st_5 from statictext within w_mant_aprobacion_faenas
end type
end forward

global type w_mant_aprobacion_faenas from w_mant_tabla
integer width = 3209
integer height = 1988
string title = "APROBACIÓN DIARIA DE FAENAS"
boolean resizable = false
st_1 st_1
st_4 st_4
dw_entidad dw_entidad
em_fecha em_fecha
cbx_todos cbx_todos
dw_remu dw_remu
st_2 st_2
dw_conexi dw_conexi
sle_rut sle_rut
cb_buscar cb_buscar
sle_nombre sle_nombre
dw_fechas dw_fechas
st_3 st_3
uo_selcliente uo_selcliente
dw_persona dw_persona
cb_1 cb_1
cb_2 cb_2
cb_3 cb_3
cb_4 cb_4
st_7 st_7
st_6 st_6
sle_mensa sle_mensa
dw_destinatarios dw_destinatarios
st_5 st_5
end type
global w_mant_aprobacion_faenas w_mant_aprobacion_faenas

type variables
DataWindowChild	idwc_entidad
Transaction			it_sqlremu

Integer				ii_tipo, ii_orden
String					is_rut, is_Aprobados[], is_fechas[]
Decimal				id_porcafp
Long					il_nrotar

uo_conectividad	iuo_conexion

//w_mant_deta_cajasembavalfaenas	iw_mantencion
end variables

forward prototypes
public function integer existeentidad (string as_rut)
public subroutine actualizaestado (integer ai_estado)
public subroutine habilitaencab (boolean ab_habilita)
public function boolean enviacorreo (integer ai_estado)
end prototypes

public function integer existeentidad (string as_rut);String 	ls_rut, ls_nombre, ls_apepat, ls_apemat

select 	pers_rutemp, pers_nombre, pers_apepat, pers_apemat, pers_nrotar
into 		:ls_rut, 	 :ls_nombre,  :ls_apepat,  :ls_apemat,  :il_nrotar
from 		dbo.remupersonal
where	pers_rutemp = :as_rut;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla remupersonal" )
	RETURN -1
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error", "El Rut ingresado no pertenece a un trabajador de Rio Blanco." + &
								"~r~nIngrese o seleccione otro", StopSign!)
	Return -1
END IF


sle_nombre.Text	=	ls_apepat + ' ' + ls_apemat + ' ' + ls_nombre

RETURN 1
end function

public subroutine actualizaestado (integer ai_estado);Long		ll_filapers, ll_filafecs
Integer	li_cliente
String	ls_rut, ls_rutant
Date		ld_fecha

SetPointer(HourGlass!)

li_cliente	=	uo_SelCliente.Codigo
ls_rutant	=	''

FOR ll_filaPers = 1 TO dw_persona.RowCount()
	
	IF dw_persona.IsSelected(ll_filaPers) THEN	
		ls_rut	=	dw_persona.Object.pers_codigo[ll_filaPers]
		
		FOR ll_filaFecs = 1 TO dw_fechas.RowCount()
			
			IF dw_fechas.IsSelected(ll_filaFecs) THEN	
				ld_fecha	=	dw_fechas.Object.fava_fecval[ll_filaFecs]
						
				DECLARE Actualiza PROCEDURE FOR dbo.fgran_actualiza_faenas  
					@Estado 	= :ai_estado,   
					@Codigo 	= :ls_rut,   
					@fecha 	= :ld_fecha,   
					@cliente	= :li_cliente
					USING SQLCA;

				EXECUTE Actualiza;
				
				IF sqlca.SQLCode = -1 THEN
					F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado " + &
													"fgran_actualiza_faenas" )
											
					sle_mensa.text	=	"Error al Actualizar Datos."
					Return
				ELSE
					sle_mensa.text	=	"Datos Actualizados Satisfactoriamente."
					
				END IF
			END IF
		NEXT
	END IF
NEXT

IF LEN(sle_mensa.text) > 0 THEN EnviaCorreo(ai_estado)
end subroutine

public subroutine habilitaencab (boolean ab_habilita);IF ab_habilita THEN
	uo_selcliente.Enabled	=	True
	em_fecha.Enabled			=	True
	sle_rut.Enabled			=	True
	cb_buscar.Enabled			=	True
	cbx_todos.Enabled			=	True
	pb_lectura.Enabled		=	True
	pb_eliminar.Enabled		=	False
	pb_grabar.Enabled			=	False
	
	dw_persona.Reset()
	dw_fechas.Reset()
	
ELSE
	uo_selcliente.Enabled	=	False
	em_fecha.Enabled			=	False
	sle_rut.Enabled			=	False
	cb_buscar.Enabled			=	False
	cbx_todos.Enabled			=	False
	pb_lectura.Enabled		=	False
	pb_eliminar.Enabled		=	True
	pb_grabar.Enabled			=	True
	
END IF
end subroutine

public function boolean enviacorreo (integer ai_estado);Long 		ll_resultado, li_filas
Integer	li_tipo
String 	sErrorMsg, ls_para, ls_cc, ls_cco, ls_persona, ls_fecha, ls_mensaje, ls_titulo, ls_nombre

sle_mensa.text	=	"Enviando aviso de aprobación por correo."

li_filas	=	dw_destinatarios.Retrieve(1)

IF li_filas < 1 THEN
	Messagebox("Error", "No Existen destinatarios para el correo", StopSign!)
	Return False
	
END IF

FOR li_filas = 1 TO dw_destinatarios.RowCount()
	li_tipo	=	dw_destinatarios.Object.sppc_tipcor[li_filas]
	
	CHOOSE CASE li_tipo
		CASE 1
			ls_para	=	ls_para 	+ "<" + dw_destinatarios.Object.sppc_correo[li_filas] + ">;"
			
		CASE 2
			ls_cc		=	ls_cc 	+ "<" + dw_destinatarios.Object.sppc_correo[li_filas] + ">;"
			
		CASE 3
			ls_cco	=	ls_cco 	+ "<" + dw_destinatarios.Object.sppc_correo[li_filas] + ">;"
			
	END CHOOSE
NEXT

FOR li_filas = 1 TO dw_persona.RowCount()
	IF dw_persona.IsSelected(li_filas) THEN
		ls_nombre	=	"(" + dw_persona.Object.pers_rutemp[li_filas] + ")"
		ls_nombre	=	ls_nombre + " " + dw_persona.Object.pers_apepat[li_filas]
		ls_nombre	=	ls_nombre + " " + dw_persona.Object.pers_apemat[li_filas]
		ls_nombre	=	ls_nombre + " " + dw_persona.Object.pers_nombre[li_filas]
		
		IF NOT IsNull(ls_nombre) THEN
			ls_persona	=	ls_persona + ls_nombre + ".~r~n"
			
		END IF
		
	END IF
	
NEXT

FOR li_filas = 1 TO dw_fechas.RowCount()
	IF dw_fechas.IsSelected(li_filas) THEN
		ls_fecha		=	ls_fecha + String(dw_fechas.Object.fava_fecval[li_filas], 'dd/mm/yyyy') + ".~r~n"
		
	END IF
	
NEXT



IF ai_estado = 1 THEN
	ls_titulo	=	"Aviso Aprobación de Faenas"
	ls_mensaje	=	"Han sido aprobadas las faenas realizadas en las fechas: ~r~n" + ls_fecha + "~r~nPara el siguiente personal:~r~n" + ls_persona
ELSE
	ls_titulo	=	"Aviso Cancelación de Faenas"
	ls_mensaje	=	"Han sido canceladas las faenas realizadas en las fechas: ~r~n" + ls_fecha + "~r~nPara el siguiente personal:~r~n" + ls_persona
END IF

//ll_resultado = send_mail("smtp.rioblanco.cl","<FrutaGranel@RioBlanco.cl>", ls_para, ls_cc, ls_cco, ls_titulo, ls_mensaje, "", sErrorMsg)
	
IF (ll_resultado < 0) THEN
	sle_mensa.text	=	"Envio de correo Fallido."
	Messagebox("Error No" + String(ll_resultado), sErrorMsg)
	Return False
ELSE
	sle_mensa.text	=	"Envio de correo exitoso."
END IF

Return True
	
end function

event open;/*
istr_mant.argumento[1]	=	Mes proceso
istr_mant.argumento[2]	=	Empleado
istr_mant.argumento[3]	=	Conexion
*/
Boolean	lb_cerrar

This.Height	= 1993
im_menu	= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_1.SetRowFocusIndicator(Hand!)
							
iuo_conexion				=	Create uo_conectividad

IF IsNull(uo_SelCliente.Codigo) 	THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
END IF

uo_SelCliente.Seleccion(False, False)
uo_SelCliente.Todos(False)
uo_SelCliente.Inicia(gi_CodExport)

dw_entidad.SetTransObject(sqlca)
dw_entidad.GetChild("zona_codigo", idwc_entidad)
idwc_entidad.SetTransObject(sqlca)
idwc_entidad.Retrieve()
dw_entidad.InsertRow(0)

dw_conexi.SetTransObject(sqlca)
dw_conexi.InsertRow(0)

dw_persona.SetTransObject(sqlca)
dw_fechas.SetTransObject(sqlca)
dw_destinatarios.SetTransObject(sqlca)

istr_mant.dw				= 	dw_1

em_fecha.Text				=	String(today(), 'mm/yyyy')
istr_mant.argumento[1]	=	'01/' + em_fecha.Text
is_rut						=	'-1'
istr_mant.argumento[2]	=	'-1'

buscar						= 	"Codigo Persona:Npers_codigo,Ape.Pat:Spers_apepat,Ape.Mat:Spers_apmpat,Nombre:Spers_nombre"
ordenar						= 	"Codigo Persona:pers_codigo,Ape.Pat:pers_apepat,Ape.Mat:pers_apemat,Nombre:pers_nombre"

cbx_todos.TriggerEvent(clicked!)
istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
end event

event ue_recuperadatos;Long		ll_fila, respuesta

w_main.SetMicroHelp("Recuperando Datos...")

sle_mensa.text	=	"Recuperando Datos..."

SetPointer(HourGlass!)
PostEvent("ue_listo")

DO
	ll_fila	= dw_persona.Retrieve(date(istr_mant.argumento[1]), is_rut, uo_SelCliente.Codigo)

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		
		ll_fila	= dw_fechas.Retrieve(date(istr_mant.argumento[1]), is_rut, uo_SelCliente.Codigo)
	
		IF ll_fila = -1 THEN
			respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
											Information!, RetryCancel!)
		ELSEIF ll_fila > 0 THEN
			dw_persona.SetFocus()
			il_fila					= 1
	
			HabilitaEncab(False)
		ELSE
			MessageBox(	"Alerta", "No existe información para los datos ingresados.", &
											Information!)
		END IF
	ELSE
		MessageBox(	"Alerta", "No existe información para los datos ingresados.", &
										Information!)
	END IF
	
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_aprobacion_faenas.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_4=create st_4
this.dw_entidad=create dw_entidad
this.em_fecha=create em_fecha
this.cbx_todos=create cbx_todos
this.dw_remu=create dw_remu
this.st_2=create st_2
this.dw_conexi=create dw_conexi
this.sle_rut=create sle_rut
this.cb_buscar=create cb_buscar
this.sle_nombre=create sle_nombre
this.dw_fechas=create dw_fechas
this.st_3=create st_3
this.uo_selcliente=create uo_selcliente
this.dw_persona=create dw_persona
this.cb_1=create cb_1
this.cb_2=create cb_2
this.cb_3=create cb_3
this.cb_4=create cb_4
this.st_7=create st_7
this.st_6=create st_6
this.sle_mensa=create sle_mensa
this.dw_destinatarios=create dw_destinatarios
this.st_5=create st_5
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.dw_entidad
this.Control[iCurrent+4]=this.em_fecha
this.Control[iCurrent+5]=this.cbx_todos
this.Control[iCurrent+6]=this.dw_remu
this.Control[iCurrent+7]=this.st_2
this.Control[iCurrent+8]=this.dw_conexi
this.Control[iCurrent+9]=this.sle_rut
this.Control[iCurrent+10]=this.cb_buscar
this.Control[iCurrent+11]=this.sle_nombre
this.Control[iCurrent+12]=this.dw_fechas
this.Control[iCurrent+13]=this.st_3
this.Control[iCurrent+14]=this.uo_selcliente
this.Control[iCurrent+15]=this.dw_persona
this.Control[iCurrent+16]=this.cb_1
this.Control[iCurrent+17]=this.cb_2
this.Control[iCurrent+18]=this.cb_3
this.Control[iCurrent+19]=this.cb_4
this.Control[iCurrent+20]=this.st_7
this.Control[iCurrent+21]=this.st_6
this.Control[iCurrent+22]=this.sle_mensa
this.Control[iCurrent+23]=this.dw_destinatarios
this.Control[iCurrent+24]=this.st_5
end on

on w_mant_aprobacion_faenas.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_4)
destroy(this.dw_entidad)
destroy(this.em_fecha)
destroy(this.cbx_todos)
destroy(this.dw_remu)
destroy(this.st_2)
destroy(this.dw_conexi)
destroy(this.sle_rut)
destroy(this.cb_buscar)
destroy(this.sle_nombre)
destroy(this.dw_fechas)
destroy(this.st_3)
destroy(this.uo_selcliente)
destroy(this.dw_persona)
destroy(this.cb_1)
destroy(this.cb_2)
destroy(this.cb_3)
destroy(this.cb_4)
destroy(this.st_7)
destroy(this.st_6)
destroy(this.sle_mensa)
destroy(this.dw_destinatarios)
destroy(this.st_5)
end on

event resize;//Integer		li_posi_y, li_objeto
//Long			ll_width
//
//ll_width		=	This.WorkSpaceWidth() - 510
//
//dw_persona.Resize(ll_width * 0.7225,This.WorkSpaceHeight() - dw_persona.y - 75)
//dw_fechas.Resize(ll_width * 0.2720,This.WorkSpaceHeight() - dw_fechas.y - 75)
//
//dw_persona.x			= 	78
//dw_fechas.x				=	78 + (ll_width * 0.728)
//
//st_encabe.width		= 	This.WorkSpaceWidth() - 510
//st_5.x					=	78
//st_5.width				= 	This.WorkSpaceWidth() - 510
//st_7.x					=	78
//st_7.width				= 	ll_width * 0.7225
//st_6.x					=	78 + (ll_width * 0.7225)
//st_6.width				= 	ll_width * 0.2775
//cb_4.x					=	st_6.x + 73
//cb_3.x					=	st_6.x + 357
//
//
////gb_1.x 					= 	This.WorkSpaceWidth() - 351
////gb_1.y 					= 	33
////gb_1.width				= 	275
////gb_1.height				= 	180 * 1 + 97 /*  (1 Botón)  */
//
//pb_lectura.x			= 	This.WorkSpaceWidth() - 292
////pb_lectura.y			= 	gb_1.y + 88
//pb_lectura.width		= 	156
//pb_lectura.height		= 	133
//
////gb_2.x 					= 	This.WorkSpaceWidth() - 351
////gb_2.width				= 	275	
//
//pb_nuevo.x				= 	This.WorkSpaceWidth() - 292
//pb_nuevo.width			= 	156
//pb_nuevo.height		= 	133
//
//pb_insertar.x			= 	This.WorkSpaceWidth() - 292
//pb_insertar.width		= 	156
//pb_insertar.height	= 	133
//
//pb_eliminar.x			= 	This.WorkSpaceWidth() - 292
//pb_eliminar.width		= 	156
//pb_eliminar.height	= 	133
//
//pb_grabar.x				= 	This.WorkSpaceWidth() - 292
//pb_grabar.width		= 	156
//pb_grabar.height		= 	133
//
//pb_imprimir.x			= 	This.WorkSpaceWidth() - 292
//pb_imprimir.width		= 	156
//pb_imprimir.height	= 	133
//
////IF st_encabe.Visible THEN
////	gb_2.y 				= 	dw_persona.y - 36
////ELSE
////	gb_2.y 				= 	gb_1.y + gb_1.height + 23
////END IF
//
////li_posi_y				= 	gb_2.y - 92
//
//IF pb_nuevo.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_nuevo.y	= li_posi_y
//END IF
//
//IF pb_insertar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_insertar.y	= li_posi_y
//END IF
//
//IF pb_eliminar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_eliminar.y	= li_posi_y
//END IF
//
//IF pb_grabar.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_grabar.y		= li_posi_y
//END IF
//
//IF pb_imprimir.Visible THEN
//	li_objeto	++
//	li_posi_y	+= 180
//	pb_imprimir.y	= li_posi_y
//END IF
//
////gb_2.height				= 180 * li_objeto + 97
////gb_3.x 					= This.WorkSpaceWidth() - 351
////gb_3.y 					= This.WorkSpaceHeight() - 345
////gb_3.width				= 275
////gb_3.height				= 180 * 1 + 97 /*  (1 Botón)  */
//
//pb_salir.x				= This.WorkSpaceWidth() - 292
////pb_salir.y				= gb_3.y + 88
//pb_salir.width			= 156
//pb_salir.height		= 133
end event

event ue_listo;call super::ue_listo;sle_mensa.text	=	""
end event

event ue_ordenar;String ls_info

str_parms	parm

parm.string_arg[1]	= ordenar
parm.dw_arg				= dw_persona

OpenWithParm(w_columna_orden, parm)

ls_info	= Message.StringParm

dw_1.SetRow(1)
dw_1.SelectRow(0,false)
dw_1.SelectRow(1,true)

RETURN
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_aprobacion_faenas
boolean visible = false
integer x = 82
integer y = 60
integer width = 105
integer height = 92
integer taborder = 60
string dataobject = "dw_mues_cajasembavalfaenas"
boolean vscrollbar = false
boolean livescroll = false
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_aprobacion_faenas
boolean visible = false
integer y = 716
integer width = 2606
integer height = 524
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_aprobacion_faenas
integer x = 2830
integer taborder = 50
long backcolor = 553648127
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_aprobacion_faenas
integer x = 2830
integer y = 420
integer taborder = 70
long backcolor = 553648127
end type

event pb_nuevo::clicked;call super::clicked;HabilitaEncab(True)
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_aprobacion_faenas
boolean visible = false
integer x = 3191
integer taborder = 80
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_aprobacion_faenas
string tag = "Aprobar Datos"
integer x = 2830
integer y = 780
integer taborder = 90
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
long backcolor = 553648127
end type

event pb_eliminar::clicked;call super::clicked;ActualizaEstado(1)
end event

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_aprobacion_faenas
string tag = "Volver Transitorios"
integer x = 2830
integer y = 1024
integer taborder = 100
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Cancelar-bn.png"
long backcolor = 553648127
end type

event pb_grabar::clicked;call super::clicked;ActualizaEstado(0)
end event

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_aprobacion_faenas
boolean visible = false
integer x = 2825
integer y = 1268
integer taborder = 110
long backcolor = 553648127
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_aprobacion_faenas
integer x = 2830
integer y = 1524
integer taborder = 120
long backcolor = 553648127
end type

type st_1 from statictext within w_mant_aprobacion_faenas
integer x = 283
integer y = 188
integer width = 379
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Mes Proceso"
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_aprobacion_faenas
integer x = 283
integer y = 288
integer width = 233
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Entidad"
boolean focusrectangle = false
end type

type dw_entidad from datawindow within w_mant_aprobacion_faenas
boolean visible = false
integer x = 3131
integer y = 1896
integer width = 905
integer height = 112
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_entidadespacking"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null
String		ls_columna

SetNull(ll_null)

ls_columna =	Dwo.Name

CHOOSE CASE ls_columna
	CASE "enpa_rutper"
		is_rut	=	F_verrut(data, True)
		IF is_rut	=	""	THEN
			This.SetItem(row,ls_columna,String(ll_null))
			Return 1
		ELSE
			istr_mant.argumento[2]	=	data
		END IF
			
END CHOOSE
end event

type em_fecha from editmask within w_mant_aprobacion_faenas
integer x = 677
integer y = 180
integer width = 425
integer height = 88
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
boolean dropdowncalendar = true
long calendartextcolor = 0
long calendartitletextcolor = 0
long calendartrailingtextcolor = 8421504
long calendarbackcolor = 12632256
long calendartitlebackcolor = 10789024
end type

event modified;istr_mant.argumento[1]	=	'01/' + This.Text
end event

type cbx_todos from checkbox within w_mant_aprobacion_faenas
integer x = 2203
integer y = 276
integer width = 402
integer height = 80
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
boolean checked = true
end type

event clicked;IF this.checked THEN
	cb_buscar.Enabled		=	False
	sle_rut.Enabled		=	False
	Is_Rut					=	'-1'
ELSE
	cb_buscar.Enabled		=	True
	sle_rut.Enabled		=	True
	Is_Rut					=	''
END IF

sle_nombre.Text	=	''
sle_rut.Text		=	''
istr_mant.argumento[2]	=	''
end event

type dw_remu from uo_dw within w_mant_aprobacion_faenas
boolean visible = false
integer x = 82
integer y = 60
integer width = 105
integer height = 92
integer taborder = 0
boolean bringtotop = true
string dataobject = "dw_mues_remumovfaenaper"
boolean hscrollbar = true
end type

type st_2 from statictext within w_mant_aprobacion_faenas
boolean visible = false
integer x = 3099
integer y = 1916
integer width = 293
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Conexión"
boolean focusrectangle = false
end type

type dw_conexi from datawindow within w_mant_aprobacion_faenas
boolean visible = false
integer x = 3483
integer y = 1888
integer width = 1248
integer height = 112
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_conectividad"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer 	ll_null
String		ls_columna

SetNull(ll_null)

ls_columna =	Dwo.Name

CHOOSE CASE ls_columna
	CASE "cone_codigo"
		IF NOT iuo_conexion.Existe(Long(data), sqlca, True)THEN
			This.Object.cone_codigo[1]	=	ll_null
			Return 1
		ELSE
			istr_mant.argumento[3]	=	data
		END IF
			
END CHOOSE
end event

event itemerror;Return 1
end event

type sle_rut from singlelineedit within w_mant_aprobacion_faenas
integer x = 677
integer y = 276
integer width = 384
integer height = 80
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;Integer	li_validarut

IF Len(This.Text) < 1 THEN Return

is_rut	=	F_verrut(This.Text, True)
IF is_rut	=	""	THEN
	This.Text					=	''
	istr_mant.argumento[2]	=	''
	Return
	
END IF

li_validarut = ExisteEntidad(This.Text) 
IF li_validarut = -1 THEN
	This.Text					=	''
	istr_mant.argumento[2]	=	''
	Return
	
END IF

This.Text	=	is_rut
end event

type cb_buscar from commandbutton within w_mant_aprobacion_faenas
integer x = 1070
integer y = 272
integer width = 101
integer height = 88
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
string text = "..."
end type

event clicked;Integer 			li_null, li_validarut
String			ls_columna
Str_busqueda	lstr_busq

SetNull(li_null)

lstr_busq.argum[1]	=	''

OpenWithParm(w_busc_remupersonal, lstr_busq)
lstr_busq				=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> '' THEN
	li_validarut = ExisteEntidad(lstr_busq.argum[1]) 
	IF li_validarut = -1 THEN
		sle_nombre.Text	=	String(li_null)
		sle_rut.Text		=	String(li_null)
		Return 1
	ELSE
		sle_rut.Text		=	lstr_busq.argum[1]
		is_rut				=	lstr_busq.argum[1]
	END IF
END IF
end event

type sle_nombre from singlelineedit within w_mant_aprobacion_faenas
integer x = 1179
integer y = 276
integer width = 1010
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type dw_fechas from uo_dw within w_mant_aprobacion_faenas
integer x = 1970
integer y = 596
integer height = 1240
integer taborder = 21
boolean bringtotop = true
string dataobject = "dw_mues_cajasembavalfaenas_fechas"
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row = 0 THEN Return

This.SelectRow(row, NOT This.IsSelected(row))
end event

type st_3 from statictext within w_mant_aprobacion_faenas
integer x = 283
integer y = 96
integer width = 379
integer height = 56
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_mant_aprobacion_faenas
integer x = 677
integer y = 80
integer width = 882
integer height = 88
integer taborder = 60
boolean bringtotop = true
string text = ""
long tabtextcolor = 0
long picturemaskcolor = 0
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type dw_persona from uo_dw within w_mant_aprobacion_faenas
integer x = 73
integer y = 596
integer width = 1897
integer height = 1240
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_cajasembavalfaenas_personas"
boolean livescroll = true
end type

event clicked;call super::clicked;IF Row = 0 THEN Return

This.SelectRow(row, NOT This.IsSelected(row))
end event

type cb_1 from commandbutton within w_mant_aprobacion_faenas
integer x = 210
integer y = 488
integer width = 279
integer height = 84
integer taborder = 80
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Todos"
end type

event clicked;dw_persona.SelectRow(0,True)
end event

type cb_2 from commandbutton within w_mant_aprobacion_faenas
integer x = 494
integer y = 488
integer width = 279
integer height = 84
integer taborder = 90
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Ninguno"
end type

event clicked;dw_persona.SelectRow(0,False)
end event

type cb_3 from commandbutton within w_mant_aprobacion_faenas
integer x = 2322
integer y = 488
integer width = 279
integer height = 84
integer taborder = 90
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Ninguna"
end type

event clicked;dw_fechas.SelectRow(0,False)
end event

type cb_4 from commandbutton within w_mant_aprobacion_faenas
integer x = 2039
integer y = 488
integer width = 279
integer height = 84
integer taborder = 90
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Todas"
end type

event clicked;dw_fechas.SelectRow(0,True)
end event

type st_7 from statictext within w_mant_aprobacion_faenas
integer x = 73
integer y = 460
integer width = 1897
integer height = 140
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_aprobacion_faenas
integer x = 1970
integer y = 460
integer width = 709
integer height = 140
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_mant_aprobacion_faenas
integer x = 677
integer y = 364
integer width = 1513
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type dw_destinatarios from datawindow within w_mant_aprobacion_faenas
boolean visible = false
integer x = 82
integer y = 60
integer width = 105
integer height = 92
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_paramcorreo_filtro"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_mant_aprobacion_faenas
integer x = 78
integer y = 40
integer width = 2601
integer height = 420
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

