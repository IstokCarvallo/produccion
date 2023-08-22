$PBExportHeader$w_info_resultado_proceso_productor.srw
$PBExportComments$Resultado de Proceso Productor
forward
global type w_info_resultado_proceso_productor from w_para_informes
end type
type cb_buscaorden from commandbutton within w_info_resultado_proceso_productor
end type
type em_fecha from editmask within w_info_resultado_proceso_productor
end type
type st_8 from statictext within w_info_resultado_proceso_productor
end type
type em_proceso from editmask within w_info_resultado_proceso_productor
end type
type st_7 from statictext within w_info_resultado_proceso_productor
end type
type st_6 from statictext within w_info_resultado_proceso_productor
end type
type dw_planta from datawindow within w_info_resultado_proceso_productor
end type
type st_2 from statictext within w_info_resultado_proceso_productor
end type
type ddlb_tipoproc from dropdownlistbox within w_info_resultado_proceso_productor
end type
type st_3 from statictext within w_info_resultado_proceso_productor
end type
type rb_1 from radiobutton within w_info_resultado_proceso_productor
end type
type rb_2 from radiobutton within w_info_resultado_proceso_productor
end type
type gb_3 from groupbox within w_info_resultado_proceso_productor
end type
type st_9 from statictext within w_info_resultado_proceso_productor
end type
type uo_selespe from uo_seleccion_especie within w_info_resultado_proceso_productor
end type
type uo_selvari from uo_seleccion_variedad within w_info_resultado_proceso_productor
end type
type uo_selprod from uo_seleccion_productor within w_info_resultado_proceso_productor
end type
type st_1 from statictext within w_info_resultado_proceso_productor
end type
type st_4 from statictext within w_info_resultado_proceso_productor
end type
type st_10 from statictext within w_info_resultado_proceso_productor
end type
type cbx_predio from checkbox within w_info_resultado_proceso_productor
end type
type gb_4 from groupbox within w_info_resultado_proceso_productor
end type
type st_5 from statictext within w_info_resultado_proceso_productor
end type
type rb_infoprod from radiobutton within w_info_resultado_proceso_productor
end type
type rb_infointe from radiobutton within w_info_resultado_proceso_productor
end type
type st_11 from statictext within w_info_resultado_proceso_productor
end type
type sle_seguimiento from singlelineedit within w_info_resultado_proceso_productor
end type
type st_12 from statictext within w_info_resultado_proceso_productor
end type
type dw_cliente from datawindow within w_info_resultado_proceso_productor
end type
type st_13 from statictext within w_info_resultado_proceso_productor
end type
end forward

global type w_info_resultado_proceso_productor from w_para_informes
integer width = 2651
integer height = 2148
string title = "Resultado de Proceso"
cb_buscaorden cb_buscaorden
em_fecha em_fecha
st_8 st_8
em_proceso em_proceso
st_7 st_7
st_6 st_6
dw_planta dw_planta
st_2 st_2
ddlb_tipoproc ddlb_tipoproc
st_3 st_3
rb_1 rb_1
rb_2 rb_2
gb_3 gb_3
st_9 st_9
uo_selespe uo_selespe
uo_selvari uo_selvari
uo_selprod uo_selprod
st_1 st_1
st_4 st_4
st_10 st_10
cbx_predio cbx_predio
gb_4 gb_4
st_5 st_5
rb_infoprod rb_infoprod
rb_infointe rb_infointe
st_11 st_11
sle_seguimiento sle_seguimiento
st_12 st_12
dw_cliente dw_cliente
st_13 st_13
end type
global w_info_resultado_proceso_productor w_info_resultado_proceso_productor

type variables
DataWindowChild	idwc_Planta, idwc_Cliente

uo_plantadesp		iuo_Planta
uo_especie			iuo_Especie

Integer    ii_TipoOrden, ii_cliente
end variables

forward prototypes
public function boolean noexisteordendereembalaje (long al_orden)
public function boolean noexisteordenproceso (long al_orden)
public function boolean noexisteordenreproceso (long al_orden)
end prototypes

public function boolean noexisteordendereembalaje (long al_orden);Integer	li_Especie, li_Planta, li_Estado, li_cliente
Date		ld_Fecha
Boolean	lb_Retorno = True


li_cliente  =  dw_cliente.Object.clie_codigo[1]
li_Planta	=	dw_Planta.Object.plde_codigo[1]

SELECT	espe_codigo, dinp_estado, dinp_fecdoc
	INTO	:li_Especie, :li_Estado, :ld_Fecha
	FROM	dba.spro_doctointernopack
	WHERE	plde_codigo	=	:li_Planta
	AND	dinp_tipdoc	=	6
	AND	dinp_numero	=	:al_Orden;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
ELSEIF sqlca.SQLCode <> 100 THEN
	iuo_Especie.Existe(li_Especie, False, sqlca)

//	sle_codesp.text	=	String(li_Especie, '00')
//	sle_espe.text		=	iuo_Especie.Nombre
//	em_fecha.text		=	String(Date(ldt_Fecha))
	sle_Seguimiento.Text	=	''
	
	IF li_Estado <> 1 THEN
		MessageBox("Atención", "Orden de Proceso ya se encuentra Cerrada")
	ELSE
		lb_Retorno	=	False
	END IF	
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordenproceso (long al_orden);Integer	li_Especie, li_Planta, li_Estado, li_Variedad, li_Seguimto, li_cliente
String	ls_Seguimiento[3]={'Productor','Huerto','Cuartel'} 
Date		ld_Fecha
Boolean	lb_Retorno = True
Long		ll_Productor

li_cliente  =  dw_cliente.Object.clie_codigo[1]
li_Planta	=	dw_Planta.Object.plde_codigo[1]

SELECT	espe_codigo, orpr_estado, orpr_fecpro, prod_codigo, vari_codigo, orpr_niveld
	INTO	:li_Especie, :li_Estado, :ld_Fecha, :ll_Productor, :li_Variedad, :li_Seguimto
	FROM	dba.spro_ordenproceso
	WHERE	plde_codigo	=	:li_Planta
	AND	orpr_tipord	=	4
	AND	orpr_numero	=	:al_Orden;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
ELSEIF sqlca.SQLCode = 0 THEN
	iuo_Especie.Existe(li_Especie, False, sqlca)

	uo_SelEspe.Todos(False)
	uo_SelEspe.dw_Seleccion.Object.codigo[1]	=	li_Especie
	uo_SelEspe.Codigo	=	li_Especie	
	uo_SelEspe.TriggerEvent("ue_cambio")
	
	uo_SelProd.Todos(False)

	uo_SelVari.Todos(False)	
	
	uo_SelVari.Codigo	=	li_Variedad
	uo_SelVari.dw_Seleccion.Object.codigo[1]	=	li_Variedad
	uo_SelProd.Codigo	=	ll_Productor
	uo_SelProd.dw_Seleccion.Object.codigo[1]	=	ll_Productor

	em_fecha.text			=	String(ld_Fecha)
	sle_Seguimiento.Text	=	ls_Seguimiento[li_Seguimto]
	
	lb_Retorno	=	False
END IF

RETURN lb_Retorno
end function

public function boolean noexisteordenreproceso (long al_orden);Integer	li_Especie, li_Planta, li_Estado, li_cliente
Date		ld_Fecha
Boolean	lb_Retorno = True


li_cliente  =  dw_cliente.Object.clie_codigo[1]
li_Planta	=	dw_Planta.Object.plde_codigo[1]

SELECT	espe_codigo, dinp_estado, dinp_fecdoc
	INTO	:li_Especie, :li_Estado, :ld_Fecha
	FROM	dba.spro_doctointernopack
	WHERE	plde_codigo	=	:li_Planta
	AND	dinp_tipdoc	=	5
	AND	dinp_numero	=	:al_Orden;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_doctointernopack")
ELSEIF sqlca.SQLCode <> 100 THEN
	iuo_Especie.Existe(li_Especie, False, sqlca)

//	sle_codesp.text	=	String(li_Especie, '00')
//	sle_espe.text		=	iuo_Especie.Nombre
//	em_fecha.text		=	String(Date(ldt_Fecha))
	sle_Seguimiento.Text	=	''	
	
	IF li_Estado <> 1 THEN
		MessageBox("Atención", "Orden de Proceso ya se encuentra Cerrada")
	ELSE
		lb_Retorno	=	False
	END IF	
END IF

RETURN lb_Retorno
end function

on w_info_resultado_proceso_productor.create
int iCurrent
call super::create
this.cb_buscaorden=create cb_buscaorden
this.em_fecha=create em_fecha
this.st_8=create st_8
this.em_proceso=create em_proceso
this.st_7=create st_7
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_2=create st_2
this.ddlb_tipoproc=create ddlb_tipoproc
this.st_3=create st_3
this.rb_1=create rb_1
this.rb_2=create rb_2
this.gb_3=create gb_3
this.st_9=create st_9
this.uo_selespe=create uo_selespe
this.uo_selvari=create uo_selvari
this.uo_selprod=create uo_selprod
this.st_1=create st_1
this.st_4=create st_4
this.st_10=create st_10
this.cbx_predio=create cbx_predio
this.gb_4=create gb_4
this.st_5=create st_5
this.rb_infoprod=create rb_infoprod
this.rb_infointe=create rb_infointe
this.st_11=create st_11
this.sle_seguimiento=create sle_seguimiento
this.st_12=create st_12
this.dw_cliente=create dw_cliente
this.st_13=create st_13
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.cb_buscaorden
this.Control[iCurrent+2]=this.em_fecha
this.Control[iCurrent+3]=this.st_8
this.Control[iCurrent+4]=this.em_proceso
this.Control[iCurrent+5]=this.st_7
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.dw_planta
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.ddlb_tipoproc
this.Control[iCurrent+10]=this.st_3
this.Control[iCurrent+11]=this.rb_1
this.Control[iCurrent+12]=this.rb_2
this.Control[iCurrent+13]=this.gb_3
this.Control[iCurrent+14]=this.st_9
this.Control[iCurrent+15]=this.uo_selespe
this.Control[iCurrent+16]=this.uo_selvari
this.Control[iCurrent+17]=this.uo_selprod
this.Control[iCurrent+18]=this.st_1
this.Control[iCurrent+19]=this.st_4
this.Control[iCurrent+20]=this.st_10
this.Control[iCurrent+21]=this.cbx_predio
this.Control[iCurrent+22]=this.gb_4
this.Control[iCurrent+23]=this.st_5
this.Control[iCurrent+24]=this.rb_infoprod
this.Control[iCurrent+25]=this.rb_infointe
this.Control[iCurrent+26]=this.st_11
this.Control[iCurrent+27]=this.sle_seguimiento
this.Control[iCurrent+28]=this.st_12
this.Control[iCurrent+29]=this.dw_cliente
this.Control[iCurrent+30]=this.st_13
end on

on w_info_resultado_proceso_productor.destroy
call super::destroy
destroy(this.cb_buscaorden)
destroy(this.em_fecha)
destroy(this.st_8)
destroy(this.em_proceso)
destroy(this.st_7)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_2)
destroy(this.ddlb_tipoproc)
destroy(this.st_3)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.gb_3)
destroy(this.st_9)
destroy(this.uo_selespe)
destroy(this.uo_selvari)
destroy(this.uo_selprod)
destroy(this.st_1)
destroy(this.st_4)
destroy(this.st_10)
destroy(this.cbx_predio)
destroy(this.gb_4)
destroy(this.st_5)
destroy(this.rb_infoprod)
destroy(this.rb_infointe)
destroy(this.st_11)
destroy(this.sle_seguimiento)
destroy(this.st_12)
destroy(this.dw_cliente)
destroy(this.st_13)
end on

event open;
X	=	0
Y	=	0


dw_cliente.GetChild("clie_codigo",idwc_Cliente)
idwc_Cliente.SetTransObject(SQLCA)

dw_planta.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)

IF idwc_planta.Retrieve(gi_codexport)=0 THEN
	idwc_planta.InsertRow(0)
END IF


dw_cliente.SetTransObject(SQLCA)
dw_cliente.InsertRow(0)

dw_planta.SetTransObject(SQLCA)
dw_planta.InsertRow(0)

//dw_planta.SetItem(1, "plde_codigo", gstr_paramplanta.CodigoPlanta)

em_fecha.text	=	String(Date(Today()))

ii_TipoOrden	=	4

ddlb_tipoproc.Text = '1. - Proceso'

iuo_Especie		=	CREATE uo_Especie

Boolean	lb_Cerrar

IF IsNull(uo_SelProd.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelEspe.Codigo) THEN lb_Cerrar	=	True

IF IsNull(uo_SelVari.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelProd.Todos(True)

	uo_SelVari.Todos(True)
	
	uo_SelVari.cbx_Todos.Enabled	=	False

END IF

dw_cliente.Setitem(1,"clie_codigo",gi_codexport)

end event

type st_titulo from w_para_informes`st_titulo within w_info_resultado_proceso_productor
integer x = 78
integer y = 68
integer width = 2066
string text = "Informe Resultado de Proceso Cuadratura"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_resultado_proceso_productor
integer x = 2304
integer y = 312
integer taborder = 90
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Fila, ll_Orden
Integer  li_Planta, li_Productor, li_Especie, li_Variedad, li_ConsPredio, li_TipoInforme

li_Planta	=	dw_Planta.Object.plde_codigo[1]
ll_Orden		=	Long(em_Proceso.text)

IF IsNull(li_planta) THEN
	MessageBox("Error de Datos", "Falta el Ingreso de Planta.")
	
	RETURN 1
END IF

IF IsNull(ii_TipoOrden) THEN
	MessageBox("Error de Datos", "Falta el Ingreso del Tipo de Proceso.")
	
	RETURN 1
END IF	

IF IsNull(ll_Orden) THEN
	MessageBox("Error de Datos", "Falta el Ingreso de un Número de Orden.")
	
	RETURN 1
END IF	

IF rb_InfoProd.Checked THEN
	li_TipoInforme	=	1
ELSE
	li_TipoInforme	=	2
END IF

li_Productor		=	uo_SelProd.Codigo
li_Especie			=	uo_SelEspe.Codigo
li_Variedad			=	uo_SelVari.Codigo

IF cbx_predio.Checked THEN li_ConsPredio	= 1

istr_info.Copias	=	1

OpenWithParm(vinf, istr_info)

CHOOSE CASE ii_TipoOrden
	CASE 4
		istr_info.Titulo		=	"RESULTADO DE PROCESO PRODUCTOR"
		
	CASE 5
		istr_info.Titulo		=	"RESULTADO DE RE-PROCESO PRODUCTOR"
	
	CASE 6
		istr_info.Titulo		=	"RESULTADO DE RE-EMBALAJE PRODUCTOR"
		
END CHOOSE

vinf.dw_1.DataObject =	"dw_info_resultado_proceso"
vinf.dw_1.SetTransObject(sqlca)

ll_Fila	=	vinf.dw_1.Retrieve(li_Planta, ii_TipoOrden, ll_Orden, li_Productor, &
										 li_Especie, li_Variedad, li_ConsPredio, li_TipoInforme)

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 100')

	vinf.Visible	=	True
	vinf.Enabled	=	True
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_resultado_proceso_productor
integer x = 2304
integer y = 676
integer taborder = 140
end type

type cb_buscaorden from commandbutton within w_info_resultado_proceso_productor
integer x = 1989
integer y = 1112
integer width = 91
integer height = 76
integer taborder = 80
boolean bringtotop = true
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
		
	CASE 4
		
		lstr_busq.argum[1]	=	String(dw_planta.Object.plde_codigo[1])
		lstr_busq.argum[2]	=	"1"
		lstr_busq.argum[3]	=	string(ii_TipoOrden)
		lstr_busq.argum[6]	=	""
		
		OpenWithParm(w_busc_spro_ordenproceso, lstr_busq)
		
		lstr_busq	= Message.PowerObjectParm
		
		IF lstr_busq.argum[6] <> "" THEN
			em_proceso.text	=	lstr_busq.argum[6]
			
			IF NoExisteOrdenProceso(Long(lstr_busq.argum[6])) THEN
				 em_proceso.text=""
			END IF	
		END IF
		
		RETURN 1
		
END CHOOSE
end event

type em_fecha from editmask within w_info_resultado_proceso_productor
integer x = 517
integer y = 1228
integer width = 325
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "none"
alignment alignment = center!
boolean displayonly = true
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_8 from statictext within w_info_resultado_proceso_productor
integer x = 151
integer y = 1240
integer width = 229
integer height = 60
boolean bringtotop = true
integer textsize = -8
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

type em_proceso from editmask within w_info_resultado_proceso_productor
integer x = 1559
integer y = 1108
integer width = 384
integer height = 84
integer taborder = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;Long ll_orden

ll_orden = long(this.text)

CHOOSE CASE ii_TipoOrden
	CASE 4
		IF NoExisteOrdenProceso(ll_orden) THEN
			this.Text=""
		END IF
		
	CASE 5
		IF NoExisteOrdenReProceso(ll_orden) THEN
			this.Text=""
		END IF
	
	CASE 6
		IF NoExisteOrdendeReEmbalaje(ll_orden) THEN
			this.Text=""
		END IF
		
END CHOOSE
end event

type st_7 from statictext within w_info_resultado_proceso_productor
integer x = 1198
integer y = 1120
integer width = 347
integer height = 60
boolean bringtotop = true
integer textsize = -8
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

type st_6 from statictext within w_info_resultado_proceso_productor
integer x = 411
integer y = 556
integer width = 370
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_resultado_proceso_productor
integer x = 955
integer y = 540
integer width = 882
integer height = 92
integer taborder = 20
boolean bringtotop = true
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

type st_2 from statictext within w_info_resultado_proceso_productor
integer x = 151
integer y = 1128
integer width = 347
integer height = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type ddlb_tipoproc from dropdownlistbox within w_info_resultado_proceso_productor
integer x = 517
integer y = 1116
integer width = 640
integer height = 328
integer taborder = 50
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 1090519039
string text = "none"
string item[] = {"Todos","Proceso","Re-Proceso","Re-Embalaje","Consolidado"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE index
		
	CASE 1
		ii_TipoOrden				=	-1
		em_proceso.Enabled		=	False
		cb_buscaorden.Enabled	=	False
		
	CASE 2,3,4
		ii_TipoOrden	=	index + 2
		em_proceso.Enabled		=	True
		cb_buscaorden.Enabled	=	True
		
	CASE 5
		ii_TipoOrden	=	-9
		em_proceso.Enabled		=	False
		cb_buscaorden.Enabled	=	False
		
END CHOOSE
end event

type st_3 from statictext within w_info_resultado_proceso_productor
integer x = 82
integer y = 484
integer width = 2066
integer height = 200
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

type rb_1 from radiobutton within w_info_resultado_proceso_productor
integer x = 411
integer y = 756
integer width = 585
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Proceso Específico"
boolean checked = true
end type

type rb_2 from radiobutton within w_info_resultado_proceso_productor
integer x = 1166
integer y = 756
integer width = 672
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Resumen de Procesos"
end type

type gb_3 from groupbox within w_info_resultado_proceso_productor
integer x = 320
integer y = 688
integer width = 1595
integer height = 180
integer taborder = 30
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Tipo de Selección"
end type

type st_9 from statictext within w_info_resultado_proceso_productor
integer x = 82
integer y = 1100
integer width = 2066
integer height = 832
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

type uo_selespe from uo_seleccion_especie within w_info_resultado_proceso_productor
event destroy ( )
integer x = 512
integer y = 1340
integer taborder = 100
boolean bringtotop = true
end type

on uo_selespe.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

ii_cliente = dw_cliente.Object.clie_codigo[1]
CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_SelVari.Todos(True)
		
		uo_SelVari.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_SelVari.Filtra(This.Codigo)
		
		uo_SelVari.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type uo_selvari from uo_seleccion_variedad within w_info_resultado_proceso_productor
event destroy ( )
integer x = 512
integer y = 1528
integer taborder = 110
boolean bringtotop = true
end type

on uo_selvari.destroy
call uo_seleccion_variedad::destroy
end on

type uo_selprod from uo_seleccion_productor within w_info_resultado_proceso_productor
event destroy ( )
integer x = 512
integer y = 1716
integer taborder = 120
boolean bringtotop = true
end type

on uo_selprod.destroy
call uo_seleccion_productor::destroy
end on

event ue_cambio();call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_SelVari.Todos(True)
		
		uo_SelVari.cbx_Todos.Enabled	=	False
		
	CASE ELSE
		uo_SelVari.Filtra(This.Codigo)
		
		uo_SelVari.cbx_Todos.Enabled	=	True
		
END CHOOSE
end event

type st_1 from statictext within w_info_resultado_proceso_productor
integer x = 151
integer y = 1348
integer width = 347
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Especie"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_resultado_proceso_productor
integer x = 151
integer y = 1536
integer width = 347
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Variedad"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_resultado_proceso_productor
integer x = 151
integer y = 1728
integer width = 347
integer height = 60
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_predio from checkbox within w_info_resultado_proceso_productor
integer x = 1527
integer y = 1804
integer width = 553
integer height = 68
integer taborder = 130
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Consolida Predios"
end type

type gb_4 from groupbox within w_info_resultado_proceso_productor
integer x = 320
integer y = 884
integer width = 1595
integer height = 180
integer taborder = 40
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Tipo de Informe"
end type

type st_5 from statictext within w_info_resultado_proceso_productor
integer x = 82
integer y = 684
integer width = 2066
integer height = 412
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

type rb_infoprod from radiobutton within w_info_resultado_proceso_productor
integer x = 411
integer y = 952
integer width = 617
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Informe al Productor"
boolean checked = true
end type

type rb_infointe from radiobutton within w_info_resultado_proceso_productor
integer x = 1166
integer y = 952
integer width = 480
integer height = 76
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Informe Interno"
end type

type st_11 from statictext within w_info_resultado_proceso_productor
integer x = 1198
integer y = 1240
integer width = 347
integer height = 56
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Seguimiento"
boolean focusrectangle = false
end type

type sle_seguimiento from singlelineedit within w_info_resultado_proceso_productor
integer x = 1559
integer y = 1228
integer width = 384
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
borderstyle borderstyle = stylelowered!
end type

type st_12 from statictext within w_info_resultado_proceso_productor
integer x = 82
integer y = 284
integer width = 2066
integer height = 200
boolean bringtotop = true
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

type dw_cliente from datawindow within w_info_resultado_proceso_productor
integer x = 814
integer y = 332
integer width = 1157
integer height = 100
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

type st_13 from statictext within w_info_resultado_proceso_productor
integer x = 411
integer y = 368
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
string text = "Cliente"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

