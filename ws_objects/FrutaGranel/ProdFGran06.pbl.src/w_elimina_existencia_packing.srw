$PBExportHeader$w_elimina_existencia_packing.srw
$PBExportComments$Genera Datos para Pronóstico de Cierre
forward
global type w_elimina_existencia_packing from window
end type
type dw_2 from uo_dw within w_elimina_existencia_packing
end type
type st_4 from statictext within w_elimina_existencia_packing
end type
type dw_cliente from uo_dw within w_elimina_existencia_packing
end type
type rb_promedio from radiobutton within w_elimina_existencia_packing
end type
type rb_envase from radiobutton within w_elimina_existencia_packing
end type
type gb_peso from groupbox within w_elimina_existencia_packing
end type
type sle_espe from singlelineedit within w_elimina_existencia_packing
end type
type sle_codesp from singlelineedit within w_elimina_existencia_packing
end type
type cb_buscaorden from commandbutton within w_elimina_existencia_packing
end type
type em_fecha from editmask within w_elimina_existencia_packing
end type
type st_8 from statictext within w_elimina_existencia_packing
end type
type em_proceso from editmask within w_elimina_existencia_packing
end type
type st_7 from statictext within w_elimina_existencia_packing
end type
type ddlb_tipoproc from dropdownlistbox within w_elimina_existencia_packing
end type
type st_6 from statictext within w_elimina_existencia_packing
end type
type dw_planta from datawindow within w_elimina_existencia_packing
end type
type sle_mensa from singlelineedit within w_elimina_existencia_packing
end type
type st_5 from statictext within w_elimina_existencia_packing
end type
type pb_salir from picturebutton within w_elimina_existencia_packing
end type
type st_2 from statictext within w_elimina_existencia_packing
end type
type st_1 from statictext within w_elimina_existencia_packing
end type
type st_titulo from statictext within w_elimina_existencia_packing
end type
type gb_2 from groupbox within w_elimina_existencia_packing
end type
type gb_1 from groupbox within w_elimina_existencia_packing
end type
type r_1 from rectangle within w_elimina_existencia_packing
end type
type st_3 from statictext within w_elimina_existencia_packing
end type
type pb_acepta from picturebutton within w_elimina_existencia_packing
end type
type dw_3 from datawindow within w_elimina_existencia_packing
end type
type dw_1 from datawindow within w_elimina_existencia_packing
end type
end forward

global type w_elimina_existencia_packing from window
integer x = 1074
integer y = 484
integer width = 2501
integer height = 1212
boolean titlebar = true
string title = "ELIMINACIÓN DE EXISTENCIA DE CAJAS EN PACKING"
long backcolor = 12632256
event ue_validapassword ( )
event ue_imprimir ( )
dw_2 dw_2
st_4 st_4
dw_cliente dw_cliente
rb_promedio rb_promedio
rb_envase rb_envase
gb_peso gb_peso
sle_espe sle_espe
sle_codesp sle_codesp
cb_buscaorden cb_buscaorden
em_fecha em_fecha
st_8 st_8
em_proceso em_proceso
st_7 st_7
ddlb_tipoproc ddlb_tipoproc
st_6 st_6
dw_planta dw_planta
sle_mensa sle_mensa
st_5 st_5
pb_salir pb_salir
st_2 st_2
st_1 st_1
st_titulo st_titulo
gb_2 gb_2
gb_1 gb_1
r_1 r_1
st_3 st_3
pb_acepta pb_acepta
dw_3 dw_3
dw_1 dw_1
end type
global w_elimina_existencia_packing w_elimina_existencia_packing

type variables
str_busqueda						istr_busq
Str_info								lstr_info
Str_mant								istr_mant
Str_info								istr_info

w_informes_con_pregunta			i_vinf

DataWindowChild					idwc_Planta, idwc_cliente

uo_plantadesp						iuo_Planta
uo_especie							iuo_Especie
uo_control_historico_proceso	iuo_historico
uo_spro_ordenproceso				iuo_proceso

Integer    							ii_TipoOrden, ii_tipord
end variables

forward prototypes
public subroutine habilitagrabar ()
public function boolean reprocesaorden (integer ai_tipoorden, long al_orden)
public function boolean noexisteordenreproceso (long al_orden)
public function boolean noexisteordenproceso (long al_orden)
public function boolean noexisteordendereembalaje (long al_orden)
public function boolean historial ()
public subroutine elimina_existencia ()
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel - Genera Cuadratura Proceso"
lstr_mant.Argumento[2]	=	gstr_paramplanta.passpack

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "ELIMINACION DE EXISTENCIA DE PACKING"
istr_info.copias	= 1

OpenWithParm(i_vinf,istr_info)

i_vinf.dw_1.DataObject = "dw_info_cajas_existencia_para_eliminacion" 

i_vinf.dw_1.SetTransObject(sqlca)


Fila = i_vinf.dw_1.Retrieve(Long(istr_mant.Argumento[1]), &
								  Long(istr_mant.Argumento[2]), &
								  Long(istr_mant.Argumento[3]), &
								  Long(istr_mant.Argumento[4]))
								  
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(i_vinf.dw_1)
	i_vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	i_vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	i_vinf.Visible	= True
	i_vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

public subroutine habilitagrabar ();
end subroutine

public function boolean reprocesaorden (integer ai_tipoorden, long al_orden);Integer	li_Planta, li_Estado, li_Cuenta
Boolean	lb_Retorno

li_Planta	=	dw_Planta.Object.plde_codigo[1]

SELECT	Count(*)
	INTO	:li_Cuenta
	FROM	dba.spro_resultpacking
	WHERE	plde_codigo	=	:li_Planta
	AND	orpr_tipdoc	=	:ai_TipoOrden
	AND	orpr_numero	=	:al_Orden ;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tabla spro_resultpacking")
ELSEIF li_Cuenta > 0 THEN
	IF MessageBox("Atención", "Proceso Seleccionado ya Posee Cuadratura." + &
		"~r~rDesea reprocesar Cuadratura .", Question!, YesNo!, 2) = 1 THEN
	
		lb_Retorno	=	True
	END IF
ELSE
	lb_Retorno	=	True
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordenreproceso (long al_orden);Integer	li_Especie, li_Planta, li_Estado, li_cliente
Date  	ldt_Fecha
Boolean	lb_Retorno = True

li_Planta	=	dw_Planta.Object.plde_codigo[1]
li_cliente	=	dw_Planta.Object.clie_codigo[1]

SELECT	espe_codigo, dinp_estado, dinp_fecdoc
	INTO	:li_Especie, :li_Estado, :ldt_Fecha
	FROM	dba.spro_doctointernopack
	WHERE	plde_codigo	=	:li_Planta
	AND	dinp_tipdoc	=	5
	AND	dinp_numero	=	:al_Orden
	AND 	clie_codigo =	:li_cliente;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca, "Lectura tabla spro_doctointernopack")

ELSEIF sqlca.SQLCode <> 100 THEN
	
	sle_codesp.text	=	String(li_Especie, '00')
	sle_espe.text		=	iuo_Especie.Nombre
	em_fecha.text		=	String(ldt_Fecha)
	
	IF li_Estado = 3 THEN
		MessageBox("Atención", "Orden de Proceso ya se encuentra Cerrada")
		
	ELSEIF li_Estado = 5 THEN
		MessageBox("Atención", "Orden de Proceso Cerrada Para Web.")
		
	ELSE
		lb_Retorno	=	False
	END IF	
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordenproceso (long al_orden);Integer	li_Especie, li_Planta, li_Estado, li_cliente
Date  	ldt_Fecha
Boolean	lb_Retorno = True

li_Planta	=	dw_Planta.Object.plde_codigo[1]
li_cliente 	= dw_cliente.Object.clie_codigo[1]

SELECT	espe_codigo, orpr_estado, orpr_fecpro
	INTO	:li_Especie, :li_Estado, :ldt_Fecha
	FROM	dba.spro_ordenproceso
	WHERE	plde_codigo	=	:li_Planta
	AND 	clie_codigo =  :li_cliente
	AND	orpr_tipord	=	:ii_tipord
	AND	orpr_numero	=	:al_Orden;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
ELSEIF sqlca.SQLCode <> 100 THEN
//	iuo_Especie.Existe(li_Especie, False, sqlca)

	sle_codesp.text	=	String(li_Especie, '00')
	sle_espe.text		=	iuo_Especie.Nombre
	em_fecha.text		=	String(ldt_Fecha)
	
	IF li_Estado = 3 THEN
		MessageBox("Atención", "Orden de Proceso ya se encuentra Cerrada")
	ELSEIF li_Estado = 5 THEN
		MessageBox("Atención", "Orden de Proceso Cerrada Para Web.")
	ELSE
		lb_Retorno	=	False
	END IF	
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordendereembalaje (long al_orden);Integer	li_Especie, li_Planta, li_Estado, li_cliente
Date		ldt_Fecha
Boolean	lb_Retorno = True

li_Planta	=	dw_Planta.Object.plde_codigo[1]
li_cliente 	= dw_cliente.Object.clie_codigo[1]

SELECT	espe_codigo, dinp_estado, dinp_fecdoc
	INTO	:li_Especie, :li_Estado, :ldt_Fecha
	FROM	dba.spro_doctointernopack
	WHERE	plde_codigo	=	:li_Planta
	AND	dinp_tipdoc	=	6
	AND	dinp_numero	=	:al_Orden;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
ELSEIF sqlca.SQLCode <> 100 THEN
//	iuo_Especie.Existe(li_Especie, False, sqlca)

	sle_codesp.text	=	String(li_Especie, '00')
	sle_espe.text		=	iuo_Especie.Nombre
	em_fecha.text		=	String(ldt_Fecha)
	
	IF li_Estado = 3 THEN
		MessageBox("Atención", "Orden de Re - Embalaje ya se encuentra Cerrada")
	ELSEIF li_Estado = 5 THEN
		MessageBox("Atención", "Orden de Proceso Cerrada Para Web.")
	ELSE
		lb_Retorno	=	False
	END IF	
END IF

RETURN lb_Retorno
end function

public function boolean historial ();Boolean			lb_retorno	=	True

Return lb_retorno
end function

public subroutine elimina_existencia ();Integer		li_Planta, li_Especie, li_TipoOrden, li_TipoPeso, li_Cliente, li_respuesta
Long			ll_Numero
String		ls_Usuario, ls_Password, ls_nombre

Transaction	sqlproc

SetPointer(HourGlass!)

sle_mensa.text	=	""
li_Planta		=	dw_Planta.Object.plde_codigo[1]
li_Especie		=	Integer(sle_codesp.text)
ll_Numero		=	Long(em_proceso.text)
li_cliente 		= 	dw_cliente.Object.clie_codigo[1]

IF IsNull(li_Planta) THEN
	MessageBox("Atención","Debe Seleccionar Planta",Exclamation!)
	
	RETURN
END IF

IF IsNull(ii_TipoOrden) THEN
	MessageBox("Atención","Debe Seleccionar Tipo de Proceso",Exclamation!)
	RETURN
ELSEIF ii_TipoOrden = 8 THEN
	li_TipoOrden = ii_TipoOrden
ELSEIF ii_TipoOrden = 5 THEN
	li_TipoOrden = ii_TipoOrden
ELSEIF ii_TipoOrden = 7 THEN
	li_TipoOrden = ii_TipoOrden
ELSE
	li_TipoOrden	=	ii_TipoOrden + 3
END IF

istr_mant.Argumento[1]	=	String(li_cliente)
istr_mant.Argumento[2]	=	String(li_Planta)
istr_mant.Argumento[3]	=	String(li_TipoOrden)
istr_mant.Argumento[4]	=	String(ll_Numero)


IF IsNull(ll_Numero) OR ll_Numero =0 THEN
	MessageBox("Atención","Debe Seleccionar Número de Proceso.",Exclamation!)
	
	RETURN
END IF	
	
sqlproc	= create Transaction

ls_Usuario				=	sqlca.UserId
ls_Password				=	sqlca.DbPass
ls_nombre				=	ProfileString(gstr_apl.ini, gs_Base, "NombreOdbc", "")
SqlProc.Dbms			=	sqlca.Dbms
SqlProc.ServerName	=	sqlca.ServerName
SqlProc.DataBase		=	sqlca.DataBase

SqlProc.DbParm			= "Connectstring='DSN=" + ls_nombre + ";UID=" + ls_Usuario + &
							  ";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
CONNECT USING SqlProc;

IF SqlProc.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado " + &
										"FGran_elimina_existencia_Packing" )
								
		sle_mensa.text	=	"Error al Generar Datos."
ELSE
	
	li_respuesta	=	MessageBox("Advertencia", "Se dispone a eliminar la información contenida en el informe impreso,~r~n" + &
															  "¿Confirma la operación?", Question!, YesNo!)
	IF li_respuesta = 2 THEN Return
	
	DECLARE	OrdenProceso PROCEDURE FOR dba.FGran_elimina_existencia_Packing
		@Planta 			=	:li_Planta,
		@Cliente			=	:li_cliente,   
		@Tipo				=	:li_TipoOrden,
		@Numero			=	:ll_Numero
	USING SqlProc ;
			
	EXECUTE OrdenProceso;	
	
	IF SqlProc.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura del Procedimiento Almacenado " + &
										"FGran_elimina_existencia_Packing" )
								
		sle_mensa.text	=	"Error al Eliminar Datos."
	ELSE
		Historial()
		sle_mensa.text	=	"Proceso Terminado Satisfactoriamente."
	END IF	
		
	CLOSE   OrdenProceso;
END IF
	
SetPointer(Arrow!)
end subroutine

on w_elimina_existencia_packing.create
this.dw_2=create dw_2
this.st_4=create st_4
this.dw_cliente=create dw_cliente
this.rb_promedio=create rb_promedio
this.rb_envase=create rb_envase
this.gb_peso=create gb_peso
this.sle_espe=create sle_espe
this.sle_codesp=create sle_codesp
this.cb_buscaorden=create cb_buscaorden
this.em_fecha=create em_fecha
this.st_8=create st_8
this.em_proceso=create em_proceso
this.st_7=create st_7
this.ddlb_tipoproc=create ddlb_tipoproc
this.st_6=create st_6
this.dw_planta=create dw_planta
this.sle_mensa=create sle_mensa
this.st_5=create st_5
this.pb_salir=create pb_salir
this.st_2=create st_2
this.st_1=create st_1
this.st_titulo=create st_titulo
this.gb_2=create gb_2
this.gb_1=create gb_1
this.r_1=create r_1
this.st_3=create st_3
this.pb_acepta=create pb_acepta
this.dw_3=create dw_3
this.dw_1=create dw_1
this.Control[]={this.dw_2,&
this.st_4,&
this.dw_cliente,&
this.rb_promedio,&
this.rb_envase,&
this.gb_peso,&
this.sle_espe,&
this.sle_codesp,&
this.cb_buscaorden,&
this.em_fecha,&
this.st_8,&
this.em_proceso,&
this.st_7,&
this.ddlb_tipoproc,&
this.st_6,&
this.dw_planta,&
this.sle_mensa,&
this.st_5,&
this.pb_salir,&
this.st_2,&
this.st_1,&
this.st_titulo,&
this.gb_2,&
this.gb_1,&
this.r_1,&
this.st_3,&
this.pb_acepta,&
this.dw_3,&
this.dw_1}
end on

on w_elimina_existencia_packing.destroy
destroy(this.dw_2)
destroy(this.st_4)
destroy(this.dw_cliente)
destroy(this.rb_promedio)
destroy(this.rb_envase)
destroy(this.gb_peso)
destroy(this.sle_espe)
destroy(this.sle_codesp)
destroy(this.cb_buscaorden)
destroy(this.em_fecha)
destroy(this.st_8)
destroy(this.em_proceso)
destroy(this.st_7)
destroy(this.ddlb_tipoproc)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.sle_mensa)
destroy(this.st_5)
destroy(this.pb_salir)
destroy(this.st_2)
destroy(this.st_1)
destroy(this.st_titulo)
destroy(this.gb_2)
destroy(this.gb_1)
destroy(this.r_1)
destroy(this.st_3)
destroy(this.pb_acepta)
destroy(this.dw_3)
destroy(this.dw_1)
end on

event open;X	=	0
Y	=	0

dw_planta.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)

IF idwc_planta.Retrieve(gi_codexport)=0 THEN
	idwc_planta.InsertRow(0)
END IF

dw_planta.SetTransObject(SQLCA)
dw_planta.InsertRow(0)

dw_planta.SetItem(1, "plde_codigo", gstr_paramplanta.codigoplanta)

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()

dw_cliente.SetTransObject(SQLCA)
dw_cliente.InsertRow(0)

dw_cliente.Object.clie_codigo[1] = gi_CodExport

dw_1.SetTransObject(SQLCA)
dw_2.SetTransObject(SQLCA)
dw_3.SetTransObject(SQLCA)

em_fecha.text			=	String(Date(Today()),'dd/mm/yyyy')

ii_TipoOrden			=	1

ddlb_tipoproc.Text 	= 	'1. - Proceso'

iuo_Especie				=	Create uo_Especie
iuo_historico			=	Create uo_control_historico_proceso
iuo_proceso				=	Create uo_spro_ordenproceso	

IF NOT IsNull(gstr_paramplanta.passpack) AND Trim(gstr_paramplanta.passpack) <> '' THEN
	PostEvent("ue_validapassword")
END IF
end event

type dw_2 from uo_dw within w_elimina_existencia_packing
integer x = 562
integer y = 1344
integer width = 1042
integer height = 728
integer taborder = 90
boolean titlebar = true
string title = "Pallets Con Anomalías"
string dataobject = "dw_proceso_cuadratura_pallets"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_4 from statictext within w_elimina_existencia_packing
integer x = 165
integer y = 244
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_cliente from uo_dw within w_elimina_existencia_packing
integer x = 590
integer y = 240
integer width = 1152
integer height = 92
integer taborder = 10
string dataobject = "dddw_clientesprod"
boolean vscrollbar = false
boolean border = false
borderstyle borderstyle = stylelowered!
end type

type rb_promedio from radiobutton within w_elimina_existencia_packing
integer x = 1019
integer y = 1624
integer width = 887
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Kilos Promedio del Proceso"
end type

type rb_envase from radiobutton within w_elimina_existencia_packing
integer x = 233
integer y = 1624
integer width = 690
integer height = 80
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Peso de Cuadratura"
boolean checked = true
end type

type gb_peso from groupbox within w_elimina_existencia_packing
integer x = 146
integer y = 1548
integer width = 1838
integer height = 200
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Peso de Referencia"
end type

type sle_espe from singlelineedit within w_elimina_existencia_packing
integer x = 777
integer y = 708
integer width = 690
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_codesp from singlelineedit within w_elimina_existencia_packing
integer x = 590
integer y = 708
integer width = 174
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type cb_buscaorden from commandbutton within w_elimina_existencia_packing
integer x = 1015
integer y = 592
integer width = 96
integer height = 80
integer taborder = 30
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

CHOOSE CASE ii_TipoOrden
	CASE 1
		lstr_busq.argum[1]	=	String(dw_planta.Object.plde_codigo[1])
		lstr_busq.argum[2]	=	"2"
		lstr_busq.argum[3]	=	"4"
		lstr_busq.argum[6]	=	""
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF lstr_busq.argum[6] <> "" THEN
//			IF ReprocesaOrden(ii_TipoOrden + 3, Long(lstr_busq.argum[6])) THEN
				em_proceso.text	=	lstr_busq.argum[6]
				
//				IF NoExisteOrdenProceso(Long(lstr_busq.argum[6])) THEN
//					 em_proceso.text=""
//					 
//				END IF	
//			END IF
		END IF
		
		RETURN 1
		
	CASE 2
		
		lstr_Busq.Argum[1]	=	'5'	// Tipo Orden

		OpenWithParm(w_busqueda_doctointernopack, lstr_Busq)

		lstr_Busq	= Message.PowerObjectParm

		IF lstr_Busq.Argum[2] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[3]
//			IF NoExisteOrdenReProceso(Long(lstr_Busq.Argum[3])) THEN
//				em_proceso.Text=""
//			END IF
		END IF
	
	CASE 3
		
		lstr_Busq.Argum[1]	=	'6'	// Tipo Orden

		OpenWithParm(w_busqueda_doctointernopack, lstr_Busq)

		lstr_Busq	= Message.PowerObjectParm

		IF lstr_Busq.Argum[2] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[3]
//			IF NoExisteOrdendeReEmbalaje(Long(lstr_Busq.Argum[3])) THEN
//				em_proceso.Text=""
//			END IF
		END IF
		
	CASE 8
		lstr_busq.argum[1]	=	String(dw_planta.Object.plde_codigo[1])
		lstr_busq.argum[2]	=	"2"
		lstr_busq.argum[3]	=	"8"
		lstr_busq.argum[6]	=	""
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF lstr_busq.argum[6] <> "" THEN
//			IF ReprocesaOrden(ii_TipoOrden, Long(lstr_busq.argum[6])) THEN
				em_proceso.text	=	lstr_busq.argum[6]
				
//				IF NoExisteOrdenProceso(Long(lstr_busq.argum[6])) THEN
//					 em_proceso.text=""
//					 
//				END IF	
//			END IF
		END IF
		
		RETURN 1
		
END CHOOSE
end event

type em_fecha from editmask within w_elimina_existencia_packing
integer x = 1627
integer y = 588
integer width = 384
integer height = 92
integer taborder = 40
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_8 from statictext within w_elimina_existencia_packing
integer x = 1353
integer y = 596
integer width = 229
integer height = 76
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Fecha"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_proceso from editmask within w_elimina_existencia_packing
integer x = 590
integer y = 588
integer width = 384
integer height = 92
integer taborder = 20
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long 		ll_orden, ll_planta
Integer	li_especie, li_cliente, li_TipoOrden


ll_planta		=	dw_Planta.Object.plde_codigo[1]
li_Especie		=	Integer(sle_codesp.text)
ll_orden 		= 	long(this.text)
li_cliente 		= 	dw_cliente.Object.clie_codigo[1]

IF IsNull(ii_TipoOrden) THEN
	MessageBox("Atención","Debe Seleccionar Tipo de Proceso",Exclamation!)
	This.Text	=	""
	RETURN
ELSEIF ii_TipoOrden = 8 THEN
	li_TipoOrden = ii_TipoOrden
ELSEIF ii_TipoOrden = 5 THEN
	li_TipoOrden = ii_TipoOrden
ELSEIF ii_TipoOrden = 7 THEN
	li_TipoOrden = ii_TipoOrden
ELSE
	li_TipoOrden	=	ii_TipoOrden + 3
END IF

iuo_proceso.Existe(ll_planta, li_TipoOrden, ll_orden, False, SQLCa, li_cliente)
IF iuo_proceso.Estado > 3 THEN
	MessageBox("Error", "La orden de proceso no puede ser modificada")
	This.Text = ""
	Return
END IF

end event

type st_7 from statictext within w_elimina_existencia_packing
integer x = 165
integer y = 600
integer width = 370
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Nº Proceso"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type ddlb_tipoproc from dropdownlistbox within w_elimina_existencia_packing
integer x = 590
integer y = 468
integer width = 640
integer height = 360
integer taborder = 10
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
string text = "none"
string item[] = {"1. - Proceso","2. - Re Proceso","3. - Re Embalaje","4. - Pre Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE Index
		
	CASE 2
		ii_TipoOrden	=	5
		
	CASE 3
		ii_TipoOrden	=	7
		
	CASE 4
		ii_TipoOrden	=	8
		
	CASE ELSE
		ii_TipoOrden	=	Index
		
END CHOOSE

sle_mensa.text			= 	""
end event

type st_6 from statictext within w_elimina_existencia_packing
integer x = 165
integer y = 356
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_elimina_existencia_packing
integer x = 590
integer y = 352
integer width = 882
integer height = 92
boolean enabled = false
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;IF data <> '' THEN
	IF idwc_Planta.Find("plde_codigo = " + data, 1, idwc_Planta.RowCount()) = 0 THEN
		MessageBox("Atención", "Código de Planta indicado no ha sido~r" + &
						"creado en tabla respectiva.~r~rIngrese o seleccione" + &
						"otra Planta.")
		
		RETURN 1
	END IF
END IF
end event

type sle_mensa from singlelineedit within w_elimina_existencia_packing
integer x = 165
integer y = 932
integer width = 1838
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_elimina_existencia_packing
integer x = 82
integer y = 868
integer width = 1993
integer height = 224
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_elimina_existencia_packing
integer x = 2222
integer y = 688
integer width = 155
integer height = 132
integer taborder = 60
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\desarrollo\bmp\exit.bmp"
alignment htextalign = right!
end type

event clicked;Close(Parent)
end event

type st_2 from statictext within w_elimina_existencia_packing
integer x = 165
integer y = 468
integer width = 393
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type st_1 from statictext within w_elimina_existencia_packing
integer x = 165
integer y = 712
integer width = 370
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_titulo from statictext within w_elimina_existencia_packing
integer x = 82
integer y = 68
integer width = 1993
integer height = 92
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Eliminación de Existencia en Packing"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type gb_2 from groupbox within w_elimina_existencia_packing
integer x = 2162
integer y = 600
integer width = 274
integer height = 272
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type gb_1 from groupbox within w_elimina_existencia_packing
integer x = 2162
integer y = 280
integer width = 274
integer height = 272
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
end type

type r_1 from rectangle within w_elimina_existencia_packing
long linecolor = 12632256
integer linethickness = 4
integer x = 773
integer y = 648
integer width = 165
integer height = 144
end type

type st_3 from statictext within w_elimina_existencia_packing
integer x = 82
integer y = 192
integer width = 1993
integer height = 676
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_acepta from picturebutton within w_elimina_existencia_packing
event clicked pbm_bnclicked
integer x = 2222
integer y = 364
integer width = 155
integer height = 132
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\desarrollo\bmp\disksave.bmp"
string disabledname = "\desarrollo\bmp\disksavd.bmp"
alignment htextalign = right!
end type

event clicked;Integer		li_Planta, li_Especie, li_TipoOrden, li_TipoPeso, li_Cliente
Long			ll_Numero
String		ls_Usuario, ls_Password, ls_nombre

SetPointer(HourGlass!)

li_Planta		=	dw_Planta.Object.plde_codigo[1]
li_Especie		=	Integer(sle_codesp.text)
ll_Numero		=	Long(em_proceso.text)
li_cliente 		= 	dw_cliente.Object.clie_codigo[1]

IF IsNull(li_Planta) THEN
	MessageBox("Atención","Debe Seleccionar Planta",Exclamation!)
	
	RETURN
END IF

IF IsNull(ii_TipoOrden) THEN
	MessageBox("Atención","Debe Seleccionar Tipo de Proceso",Exclamation!)
	RETURN
ELSEIF ii_TipoOrden = 8 THEN
	li_TipoOrden = ii_TipoOrden
ELSEIF ii_TipoOrden = 5 THEN
	li_TipoOrden = ii_TipoOrden
ELSEIF ii_TipoOrden = 7 THEN
	li_TipoOrden = ii_TipoOrden
ELSE
	li_TipoOrden	=	ii_TipoOrden + 3
END IF

istr_mant.Argumento[1]	=	String(li_cliente)
istr_mant.Argumento[2]	=	String(li_Planta)
istr_mant.Argumento[3]	=	String(li_TipoOrden)
istr_mant.Argumento[4]	=	String(ll_Numero)

Parent.TriggerEvent("ue_imprimir")
end event

type dw_3 from datawindow within w_elimina_existencia_packing
integer x = 37
integer y = 1332
integer width = 2354
integer height = 728
integer taborder = 90
boolean titlebar = true
string title = "Detalle de Resultado sin Calibre"
string dataobject = "dw_detalle_resultado_sin_calibre"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_1 from datawindow within w_elimina_existencia_packing
integer x = 37
integer y = 1332
integer width = 2354
integer height = 728
integer taborder = 80
boolean titlebar = true
string title = "Embalajes sin Pesaje"
string dataobject = "dw_cons_embalajes_sin_pesaje"
boolean hscrollbar = true
boolean vscrollbar = true
boolean hsplitscroll = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

