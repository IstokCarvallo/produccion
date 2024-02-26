$PBExportHeader$w_info_recepcion_pallet.srw
forward
global type w_info_recepcion_pallet from w_para_informes
end type
type st_4 from statictext within w_info_recepcion_pallet
end type
type st_1 from statictext within w_info_recepcion_pallet
end type
type st_5 from statictext within w_info_recepcion_pallet
end type
type st_2 from statictext within w_info_recepcion_pallet
end type
type em_fech_ini from editmask within w_info_recepcion_pallet
end type
type st_6 from statictext within w_info_recepcion_pallet
end type
type st_3 from statictext within w_info_recepcion_pallet
end type
type st_7 from statictext within w_info_recepcion_pallet
end type
type em_nroguia from editmask within w_info_recepcion_pallet
end type
type cbx_guia from checkbox within w_info_recepcion_pallet
end type
type st_productor from statictext within w_info_recepcion_pallet
end type
type st_9 from statictext within w_info_recepcion_pallet
end type
type cbx_consolida from checkbox within w_info_recepcion_pallet
end type
type st_8 from statictext within w_info_recepcion_pallet
end type
type dw_frurecep from datawindow within w_info_recepcion_pallet
end type
type cbx_todosfru from checkbox within w_info_recepcion_pallet
end type
type cbx_consfru from checkbox within w_info_recepcion_pallet
end type
type st_10 from statictext within w_info_recepcion_pallet
end type
type em_fech_fin from editmask within w_info_recepcion_pallet
end type
type em_guia from editmask within w_info_recepcion_pallet
end type
type cbx_guias from checkbox within w_info_recepcion_pallet
end type
type st_11 from statictext within w_info_recepcion_pallet
end type
type st_12 from statictext within w_info_recepcion_pallet
end type
type cbx_reembala from checkbox within w_info_recepcion_pallet
end type
type cbx_conspredio from checkbox within w_info_recepcion_pallet
end type
type uo_selespecie from uo_seleccion_especie within w_info_recepcion_pallet
end type
type uo_selclliente from uo_seleccion_clientesprod within w_info_recepcion_pallet
end type
type uo_selproductor from uo_seleccion_productor_cliente within w_info_recepcion_pallet
end type
type uo_selplantas from uo_seleccion_plantas within w_info_recepcion_pallet
end type
type uo_selpacking from uo_seleccion_plantas within w_info_recepcion_pallet
end type
end forward

global type w_info_recepcion_pallet from w_para_informes
integer x = 14
integer y = 32
integer width = 2830
integer height = 2420
string title = "Producción en Frigorificos"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
st_5 st_5
st_2 st_2
em_fech_ini em_fech_ini
st_6 st_6
st_3 st_3
st_7 st_7
em_nroguia em_nroguia
cbx_guia cbx_guia
st_productor st_productor
st_9 st_9
cbx_consolida cbx_consolida
st_8 st_8
dw_frurecep dw_frurecep
cbx_todosfru cbx_todosfru
cbx_consfru cbx_consfru
st_10 st_10
em_fech_fin em_fech_fin
em_guia em_guia
cbx_guias cbx_guias
st_11 st_11
st_12 st_12
cbx_reembala cbx_reembala
cbx_conspredio cbx_conspredio
uo_selespecie uo_selespecie
uo_selclliente uo_selclliente
uo_selproductor uo_selproductor
uo_selplantas uo_selplantas
uo_selpacking uo_selpacking
end type
global w_info_recepcion_pallet w_info_recepcion_pallet

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro,idwc_fruta

String 	is_fecha_emb
Integer 	ii_año, ii_mes, ii_dia, ii_var
Long		il_NroGuia

uo_frutarecepcion			iuo_frutarecepcion
end variables

forward prototypes
public function boolean existepacking (integer li_planta)
public function boolean existeproductor (long ll_productor)
end prototypes

public function boolean existepacking (integer li_planta);Integer  li_codigo

SELECT	plde_codigo
INTO    :li_Codigo
FROM	dbo.plantadesp
WHERE	plde_codigo =  :li_planta;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Planta")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[8] = String(li_planta)
	RETURN True 
END IF
end function

public function boolean existeproductor (long ll_productor);String	ls_Nombre

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores as pro,dbo.productoresclientes as cli
	WHERE	pro.prod_codigo =	:ll_productor
	AND	pro.prod_codigo = cli.prod_codigo
	AND	:uo_SelClliente.Codigo in (-1,cli.clie_codigo);
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Definido o no pertenece a este cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[6] = String(ll_Productor)	
	RETURN True
END IF
end function

on w_info_recepcion_pallet.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.st_5=create st_5
this.st_2=create st_2
this.em_fech_ini=create em_fech_ini
this.st_6=create st_6
this.st_3=create st_3
this.st_7=create st_7
this.em_nroguia=create em_nroguia
this.cbx_guia=create cbx_guia
this.st_productor=create st_productor
this.st_9=create st_9
this.cbx_consolida=create cbx_consolida
this.st_8=create st_8
this.dw_frurecep=create dw_frurecep
this.cbx_todosfru=create cbx_todosfru
this.cbx_consfru=create cbx_consfru
this.st_10=create st_10
this.em_fech_fin=create em_fech_fin
this.em_guia=create em_guia
this.cbx_guias=create cbx_guias
this.st_11=create st_11
this.st_12=create st_12
this.cbx_reembala=create cbx_reembala
this.cbx_conspredio=create cbx_conspredio
this.uo_selespecie=create uo_selespecie
this.uo_selclliente=create uo_selclliente
this.uo_selproductor=create uo_selproductor
this.uo_selplantas=create uo_selplantas
this.uo_selpacking=create uo_selpacking
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.em_fech_ini
this.Control[iCurrent+6]=this.st_6
this.Control[iCurrent+7]=this.st_3
this.Control[iCurrent+8]=this.st_7
this.Control[iCurrent+9]=this.em_nroguia
this.Control[iCurrent+10]=this.cbx_guia
this.Control[iCurrent+11]=this.st_productor
this.Control[iCurrent+12]=this.st_9
this.Control[iCurrent+13]=this.cbx_consolida
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.dw_frurecep
this.Control[iCurrent+16]=this.cbx_todosfru
this.Control[iCurrent+17]=this.cbx_consfru
this.Control[iCurrent+18]=this.st_10
this.Control[iCurrent+19]=this.em_fech_fin
this.Control[iCurrent+20]=this.em_guia
this.Control[iCurrent+21]=this.cbx_guias
this.Control[iCurrent+22]=this.st_11
this.Control[iCurrent+23]=this.st_12
this.Control[iCurrent+24]=this.cbx_reembala
this.Control[iCurrent+25]=this.cbx_conspredio
this.Control[iCurrent+26]=this.uo_selespecie
this.Control[iCurrent+27]=this.uo_selclliente
this.Control[iCurrent+28]=this.uo_selproductor
this.Control[iCurrent+29]=this.uo_selplantas
this.Control[iCurrent+30]=this.uo_selpacking
end on

on w_info_recepcion_pallet.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.st_5)
destroy(this.st_2)
destroy(this.em_fech_ini)
destroy(this.st_6)
destroy(this.st_3)
destroy(this.st_7)
destroy(this.em_nroguia)
destroy(this.cbx_guia)
destroy(this.st_productor)
destroy(this.st_9)
destroy(this.cbx_consolida)
destroy(this.st_8)
destroy(this.dw_frurecep)
destroy(this.cbx_todosfru)
destroy(this.cbx_consfru)
destroy(this.st_10)
destroy(this.em_fech_fin)
destroy(this.em_guia)
destroy(this.cbx_guias)
destroy(this.st_11)
destroy(this.st_12)
destroy(this.cbx_reembala)
destroy(this.cbx_conspredio)
destroy(this.uo_selespecie)
destroy(this.uo_selclliente)
destroy(this.uo_selproductor)
destroy(this.uo_selplantas)
destroy(this.uo_selpacking)
end on

event open;call super::open;Boolean lb_Cerrar

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelClliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPacking.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	iuo_frutarecepcion		=	CREATE		uo_frutarecepcion
	
	uo_SelEspecie.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelClliente.Seleccion(True, False)
	uo_SelPlantas.Seleccion(True, False)
	uo_SelPacking.Seleccion(True, False)
	
	uo_SelClliente.Todos(False)
	
//	uo_SelEspecie.Inicia(gi_CodEspecie)
	uo_SelClliente.Inicia(gi_CodExport)
	
	uo_SelPlantas.Filtra(1)
	uo_SelPacking.Filtra(2)
	uo_SelProductor.Filtra(gi_CodExport)
	
	dw_frurecep.GetChild("frre_codigo", idwc_fruta)
	idwc_fruta.SetTransObject(SQLCA)
	idwc_fruta.Retrieve()
	dw_frurecep.InsertRow(0)
	
	em_fech_ini.Text			=	String(Today())
	em_fech_fin.Text			=	String(Today())
	is_fecha_emb				= 	em_fech_ini.Text
	il_NroGuia					=	0
	ii_var=1
	
	istr_mant.argumento[15]	=	String(Today())
	istr_mant.argumento[25]	=	String(Today())	
End If
end event

event resize;//
end event

type pb_excel from w_para_informes`pb_excel within w_info_recepcion_pallet
end type

type st_computador from w_para_informes`st_computador within w_info_recepcion_pallet
end type

type st_usuario from w_para_informes`st_usuario within w_info_recepcion_pallet
end type

type st_temporada from w_para_informes`st_temporada within w_info_recepcion_pallet
end type

type p_logo from w_para_informes`p_logo within w_info_recepcion_pallet
end type

type st_titulo from w_para_informes`st_titulo within w_info_recepcion_pallet
integer width = 2048
string text = "Recepción Diaria de Pallets"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_recepcion_pallet
string tag = "Imprimir Reporte"
integer x = 2478
integer y = 1396
integer taborder = 150
integer weight = 400
fontcharset fontcharset = ansi!
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	fila, li_fruta, li_reembala, li_conspredio
Date		ld_fecha, ld_fechafin
String		ls_nroguia, ls_descri, ls_reembala
Long    	ll_nroguia, ll_guia

istr_info.titulo	= 'RECEPCION DIARIA DE PALLETS'

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_recepcion_palletdiaria"
vinf.dw_1.SetTransObject(sqlca)

// Guia 
IF cbx_consolida.Checked THEN
	ll_nroguia 	= -9
	ls_nroguia 	= 'Recepcion : Consolidada'
ELSEIF cbx_guia.checked  THEN
	ls_nroguia  =	'Recepcion : Todas'
	ll_nroguia  =   -1
ELSE
	ll_nroguia	=	Long(em_nroguia.text)
	IF IsNull(ll_nroguia) OR ll_nroguia = 0 THEN
	   MessageBox("Atención","Debe Seleccionar una Guía Previamente",Exclamation!)
		em_nroguia.setfocus()
		RETURN
	ELSE
		ls_nroguia  = 	'Recepcion : ' + String(ll_nroguia)
   END IF 
END IF

//Caracteristica de Recepción
IF cbx_consfru.Checked THEN
	li_fruta 	= -9
ELSEIF cbx_todosfru.checked  THEN
	li_fruta  =   -1
ELSE
	li_fruta	=	dw_frurecep.Object.frre_codigo[1]
	IF IsNull(li_fruta) OR li_fruta = 0 THEN
	   MessageBox("Atención","Debe Seleccionar una Carácteristica Previamente",Exclamation!)
		dw_frurecep.setfocus()
		RETURN
	END IF
END IF

IF cbx_conspredio.Checked THEN
	li_conspredio = -9
ELSE
	li_conspredio = 0
END IF	

IF cbx_guias.Checked THEN
	ll_guia = -1
ELSE
	ll_guia = Long(em_guia.Text)
END IF	

//fecha
ld_fecha		=	Date(em_fech_ini.Text)
ld_fechafin	=	Date(em_fech_fin.Text)

IF cbx_reembala.Checked THEN
	li_reembala = 	1
	ls_reembala	=	'Incluye Reembalajes'
ELSE
	li_reembala = 0
	ls_reembala	=	''
END IF

fila	=	vinf.dw_1.Retrieve(uo_SelClliente.Codigo, uo_SelPlantas.Codigo, ld_fecha, uo_SelEspecie.Codigo, ll_nroguia, &
			uo_SelProductor.Codigo, uo_SelPacking.Codigo, li_fruta,ld_fechafin,ll_guia,li_reembala,li_conspredio)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify("guia.text = '" + ls_nroguia + "'")
	vinf.dw_1.Modify("t_reembala.text = '" + ls_reembala + "'")		
	vinf.dw_1.Modify('DataWindow.Zoom = 95')
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF
SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_recepcion_pallet
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2473
integer y = 1680
integer taborder = 160
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_recepcion_pallet
integer x = 251
integer y = 440
integer width = 2048
integer height = 772
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_1 from statictext within w_info_recepcion_pallet
integer x = 311
integer y = 732
integer width = 329
integer height = 76
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_recepcion_pallet
integer x = 251
integer y = 1216
integer width = 2048
integer height = 744
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_recepcion_pallet
integer x = 325
integer y = 1664
integer width = 517
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Recepción Desde"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fech_ini from editmask within w_info_recepcion_pallet
integer x = 855
integer y = 1656
integer width = 425
integer height = 96
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[15]	=	This.Text
end event

type st_6 from statictext within w_info_recepcion_pallet
integer x = 311
integer y = 552
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_recepcion_pallet
integer x = 325
integer y = 1868
integer width = 462
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_recepcion_pallet
integer x = 325
integer y = 1392
integer width = 462
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nº Recepción"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_nroguia from editmask within w_info_recepcion_pallet
integer x = 855
integer y = 1372
integer width = 402
integer height = 96
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;il_NroGuia = Long(This.Text)
end event

type cbx_guia from checkbox within w_info_recepcion_pallet
integer x = 1440
integer y = 1388
integer width = 279
integer height = 80
integer taborder = 90
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
end type

event clicked;call super::clicked;IF This.Checked THEN
   cbx_consolida.Checked   =  false
	em_nroguia.Enabled		=	False
	em_fech_ini.enabled=True
//	em_fech_ini.text = String(Today())
//	istr_mant.argumento[3] = String(Today())

ELSE
	em_nroguia.Enabled		=	True
	em_nroguia.SetFocus()
//	em_fech_ini.enabled=False	
//	em_fech_ini.text=String(Today())

END IF
end event

type st_productor from statictext within w_info_recepcion_pallet
integer x = 311
integer y = 904
integer width = 329
integer height = 76
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
string text = "Productor"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_recepcion_pallet
integer x = 311
integer y = 1096
integer width = 329
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Packing"
boolean focusrectangle = false
end type

type cbx_consolida from checkbox within w_info_recepcion_pallet
integer x = 1797
integer y = 1388
integer width = 443
integer height = 80
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidada"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	cbx_guia.Checked			=	False       
	em_nroguia.Enabled		=	False
	em_fech_ini.enabled=True
//	em_fech_ini.text = String(Today())
//	istr_mant.argumento[3] = String(Today())
ELSE
	cbx_guia.Checked			=	true      
	em_nroguia.Enabled		=	false
END IF
end event

type st_8 from statictext within w_info_recepcion_pallet
integer x = 325
integer y = 1532
integer width = 521
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Caráct. de Recep."
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_frurecep from datawindow within w_info_recepcion_pallet
integer x = 855
integer y = 1512
integer width = 567
integer height = 84
integer taborder = 80
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_frutarecep"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_nula

IF NOT iuo_frutarecepcion.existe(Integer(data),True,sqlca) THEN
	This.SetItem(1, "frre_codigo", li_nula)
	RETURN 1
END IF
end event

type cbx_todosfru from checkbox within w_info_recepcion_pallet
integer x = 1440
integer y = 1516
integer width = 306
integer height = 80
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
end type

event clicked;Integer li_null
SetNull(li_null)

call super::clicked;IF This.Checked THEN
   cbx_consfru.Checked   =  false
	dw_frurecep.Enabled		=	False
	dw_frurecep.Object.frre_codigo[1] = li_null
ELSE
	dw_frurecep.Enabled		=	True
	dw_frurecep.SetFocus()
END IF
end event

type cbx_consfru from checkbox within w_info_recepcion_pallet
integer x = 1797
integer y = 1516
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidada"
boolean checked = true
end type

event clicked;Integer li_Null
SetNull(li_null)

IF This.Checked THEN
	cbx_todosfru.Checked		=	False       
	dw_frurecep.Enabled		=	False
	dw_frurecep.Object.frre_codigo[1] = li_null
ELSE
	cbx_todosfru.Checked		=	true      
	dw_frurecep.Enabled		=	false
END IF
end event

type st_10 from statictext within w_info_recepcion_pallet
integer x = 1600
integer y = 1668
integer width = 210
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Hasta "
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fech_fin from editmask within w_info_recepcion_pallet
integer x = 1847
integer y = 1656
integer width = 425
integer height = 96
integer taborder = 130
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;istr_mant.argumento[25]	=	This.Text
end event

type em_guia from editmask within w_info_recepcion_pallet
integer x = 859
integer y = 1236
integer width = 402
integer height = 96
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;il_NroGuia = Long(This.Text)
end event

type cbx_guias from checkbox within w_info_recepcion_pallet
integer x = 1440
integer y = 1244
integer width = 279
integer height = 80
integer taborder = 90
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

event clicked;call super::clicked;IF This.Checked THEN
   
	em_guia.Enabled		=	False
	em_guia.Text			= ''
ELSE
	em_guia.Enabled		=	True
	em_guia.SetFocus()

END IF
end event

type st_11 from statictext within w_info_recepcion_pallet
integer x = 325
integer y = 1252
integer width = 535
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nº Guía Despacho"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_recepcion_pallet
integer x = 251
integer y = 1960
integer width = 2048
integer height = 216
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_reembala from checkbox within w_info_recepcion_pallet
integer x = 850
integer y = 1980
integer width = 1248
integer height = 80
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Incluye Recepciones por Reembalajes   "
end type

type cbx_conspredio from checkbox within w_info_recepcion_pallet
integer x = 850
integer y = 2072
integer width = 1248
integer height = 80
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolida Predios y Cuarteles"
end type

type uo_selespecie from uo_seleccion_especie within w_info_recepcion_pallet
event destroy ( )
integer x = 850
integer y = 1764
integer height = 180
integer taborder = 170
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selclliente from uo_seleccion_clientesprod within w_info_recepcion_pallet
event destroy ( )
integer x = 859
integer y = 464
integer taborder = 20
boolean bringtotop = true
end type

on uo_selclliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selproductor from uo_seleccion_productor_cliente within w_info_recepcion_pallet
event destroy ( )
integer x = 859
integer y = 824
integer taborder = 50
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor_cliente::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_info_recepcion_pallet
event destroy ( )
integer x = 859
integer y = 640
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

type uo_selpacking from uo_seleccion_plantas within w_info_recepcion_pallet
event destroy ( )
integer x = 859
integer y = 1000
integer taborder = 70
boolean bringtotop = true
end type

on uo_selpacking.destroy
call uo_seleccion_plantas::destroy
end on

