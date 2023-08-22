$PBExportHeader$w_info_packing_frigorifico.srw
$PBExportComments$Informe de Repalletizajes.
forward
global type w_info_packing_frigorifico from w_para_informes
end type
type st_4 from statictext within w_info_packing_frigorifico
end type
type st_1 from statictext within w_info_packing_frigorifico
end type
type dw_2 from datawindow within w_info_packing_frigorifico
end type
type st_6 from statictext within w_info_packing_frigorifico
end type
type dw_planta from datawindow within w_info_packing_frigorifico
end type
type st_8 from statictext within w_info_packing_frigorifico
end type
type cbx_productor from checkbox within w_info_packing_frigorifico
end type
type dw_productor from datawindow within w_info_packing_frigorifico
end type
type st_3 from statictext within w_info_packing_frigorifico
end type
type dw_especie from datawindow within w_info_packing_frigorifico
end type
type st_embalaje from statictext within w_info_packing_frigorifico
end type
type cbx_embalaje from checkbox within w_info_packing_frigorifico
end type
type em_embalaje from editmask within w_info_packing_frigorifico
end type
type cb_buscaembalaje from commandbutton within w_info_packing_frigorifico
end type
type st_9 from statictext within w_info_packing_frigorifico
end type
type em_procdesde from editmask within w_info_packing_frigorifico
end type
type st_10 from statictext within w_info_packing_frigorifico
end type
type em_prochasta from editmask within w_info_packing_frigorifico
end type
type cbx_especie from checkbox within w_info_packing_frigorifico
end type
type cbx_planta from checkbox within w_info_packing_frigorifico
end type
type st_2 from statictext within w_info_packing_frigorifico
end type
type em_desde from editmask within w_info_packing_frigorifico
end type
type st_7 from statictext within w_info_packing_frigorifico
end type
type em_hasta from editmask within w_info_packing_frigorifico
end type
type gb_4 from groupbox within w_info_packing_frigorifico
end type
type st_11 from statictext within w_info_packing_frigorifico
end type
type gb_3 from groupbox within w_info_packing_frigorifico
end type
type st_5 from statictext within w_info_packing_frigorifico
end type
type cbx_fecha from checkbox within w_info_packing_frigorifico
end type
type cbx_proceso from checkbox within w_info_packing_frigorifico
end type
type cbx_consolidado from checkbox within w_info_packing_frigorifico
end type
end forward

global type w_info_packing_frigorifico from w_para_informes
string tag = "Estadistico Resultado Proceso"
integer x = 14
integer y = 32
integer width = 2697
integer height = 1940
string title = "INFORME ESTADISTICO RESULTADO PROCESO"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_4 st_4
st_1 st_1
dw_2 dw_2
st_6 st_6
dw_planta dw_planta
st_8 st_8
cbx_productor cbx_productor
dw_productor dw_productor
st_3 st_3
dw_especie dw_especie
st_embalaje st_embalaje
cbx_embalaje cbx_embalaje
em_embalaje em_embalaje
cb_buscaembalaje cb_buscaembalaje
st_9 st_9
em_procdesde em_procdesde
st_10 st_10
em_prochasta em_prochasta
cbx_especie cbx_especie
cbx_planta cbx_planta
st_2 st_2
em_desde em_desde
st_7 st_7
em_hasta em_hasta
gb_4 gb_4
st_11 st_11
gb_3 gb_3
st_5 st_5
cbx_fecha cbx_fecha
cbx_proceso cbx_proceso
cbx_consolidado cbx_consolidado
end type
global w_info_packing_frigorifico w_info_packing_frigorifico

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_tipopro, idwc_cliente, idwc_planta,idwc_especie,idwc_productor

Integer ii_etiq, ii_TipoRepa
end variables

forward prototypes
public function boolean existeproductor (long ll_productor)
public function boolean existeespecie (integer especie)
end prototypes

public function boolean existeproductor (long ll_productor);String	ls_Nombre
Integer	li_cliente

li_cliente	= dw_2.Object.clie_Codigo[1]

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dbo.productores as pro,dbo.productoresclientes as cli
	WHERE	pro.prod_codigo =	:ll_productor
	AND	pro.prod_codigo = cli.prod_codigo
	AND	:li_cliente in (-1,cli.clie_codigo);
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Productor")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de productor no ha sido definido o no pertenece a este cliente.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	istr_mant.argumento[4] = String(ll_Productor)	
	RETURN True
END IF
end function

public function boolean existeespecie (integer especie);String		ls_Nombre

SELECT	espe_nombre
	INTO	:ls_Nombre
	FROM	dbo.especies
	WHERE	espe_codigo	=	:Especie ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Especies")
	
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
	RETURN False
ELSE
	RETURN True
END IF
end function

on w_info_packing_frigorifico.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.dw_2=create dw_2
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_8=create st_8
this.cbx_productor=create cbx_productor
this.dw_productor=create dw_productor
this.st_3=create st_3
this.dw_especie=create dw_especie
this.st_embalaje=create st_embalaje
this.cbx_embalaje=create cbx_embalaje
this.em_embalaje=create em_embalaje
this.cb_buscaembalaje=create cb_buscaembalaje
this.st_9=create st_9
this.em_procdesde=create em_procdesde
this.st_10=create st_10
this.em_prochasta=create em_prochasta
this.cbx_especie=create cbx_especie
this.cbx_planta=create cbx_planta
this.st_2=create st_2
this.em_desde=create em_desde
this.st_7=create st_7
this.em_hasta=create em_hasta
this.gb_4=create gb_4
this.st_11=create st_11
this.gb_3=create gb_3
this.st_5=create st_5
this.cbx_fecha=create cbx_fecha
this.cbx_proceso=create cbx_proceso
this.cbx_consolidado=create cbx_consolidado
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.dw_2
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.st_8
this.Control[iCurrent+7]=this.cbx_productor
this.Control[iCurrent+8]=this.dw_productor
this.Control[iCurrent+9]=this.st_3
this.Control[iCurrent+10]=this.dw_especie
this.Control[iCurrent+11]=this.st_embalaje
this.Control[iCurrent+12]=this.cbx_embalaje
this.Control[iCurrent+13]=this.em_embalaje
this.Control[iCurrent+14]=this.cb_buscaembalaje
this.Control[iCurrent+15]=this.st_9
this.Control[iCurrent+16]=this.em_procdesde
this.Control[iCurrent+17]=this.st_10
this.Control[iCurrent+18]=this.em_prochasta
this.Control[iCurrent+19]=this.cbx_especie
this.Control[iCurrent+20]=this.cbx_planta
this.Control[iCurrent+21]=this.st_2
this.Control[iCurrent+22]=this.em_desde
this.Control[iCurrent+23]=this.st_7
this.Control[iCurrent+24]=this.em_hasta
this.Control[iCurrent+25]=this.gb_4
this.Control[iCurrent+26]=this.st_11
this.Control[iCurrent+27]=this.gb_3
this.Control[iCurrent+28]=this.st_5
this.Control[iCurrent+29]=this.cbx_fecha
this.Control[iCurrent+30]=this.cbx_proceso
this.Control[iCurrent+31]=this.cbx_consolidado
end on

on w_info_packing_frigorifico.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.dw_2)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_8)
destroy(this.cbx_productor)
destroy(this.dw_productor)
destroy(this.st_3)
destroy(this.dw_especie)
destroy(this.st_embalaje)
destroy(this.cbx_embalaje)
destroy(this.em_embalaje)
destroy(this.cb_buscaembalaje)
destroy(this.st_9)
destroy(this.em_procdesde)
destroy(this.st_10)
destroy(this.em_prochasta)
destroy(this.cbx_especie)
destroy(this.cbx_planta)
destroy(this.st_2)
destroy(this.em_desde)
destroy(this.st_7)
destroy(this.em_hasta)
destroy(this.gb_4)
destroy(this.st_11)
destroy(this.gb_3)
destroy(this.st_5)
destroy(this.cbx_fecha)
destroy(this.cbx_proceso)
destroy(this.cbx_consolidado)
end on

event open;call super::open;String	ls_Planta

SELECT	plde_nombre
	INTO	:ls_Planta
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:gi_CodPlanta ;

dw_2.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_2.InsertRow(0)
dw_2.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(1)
dw_planta.InsertRow(0)
//dw_1.SetItem(1, "plde_codigo", gi_CodPlanta)

dw_productor.GetChild("prod_codigo", idwc_productor)
idwc_productor.SetTransObject(sqlca)
idwc_productor.Retrieve(gi_CodExport)
dw_productor.InsertRow(0)

dw_especie.GetChild("espe_codigo", idwc_especie)
idwc_especie.SetTransObject(sqlca)
idwc_especie.Retrieve()
dw_especie.InsertRow(0)
//dw_especie.SetItem(1, "espe_codigo", gi_CodEspecie)

em_desde.Text = String(RelativeDate(Today(),-365)) 
em_hasta.Text = String(today())

istr_mant.argumento[1]	=	'-1'		//Planta
istr_mant.argumento[2]	=	String(gi_codexport)		//cliente
istr_mant.argumento[3]	=	'-1'	//Especie
istr_mant.argumento[4]	=	"-1"							//productor
istr_mant.argumento[5]	=	"-9"							//Embalaje



end event

type pb_excel from w_para_informes`pb_excel within w_info_packing_frigorifico
integer x = 2693
end type

type st_computador from w_para_informes`st_computador within w_info_packing_frigorifico
end type

type st_usuario from w_para_informes`st_usuario within w_info_packing_frigorifico
end type

type st_temporada from w_para_informes`st_temporada within w_info_packing_frigorifico
end type

type p_logo from w_para_informes`p_logo within w_info_packing_frigorifico
end type

type st_titulo from w_para_informes`st_titulo within w_info_packing_frigorifico
integer width = 1902
string text = "Estadistico Resultado Proceso"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_packing_frigorifico
string tag = "Imprimir Reporte"
integer x = 2341
integer y = 1184
integer taborder = 180
end type

event pb_acepta::clicked;Integer	fila
Long		ll_numeroini, ll_numerofin
Date		ld_fechaini, ld_fechafin

SetPointer(Arrow!)

If cbx_embalaje.Checked and NOT cbx_consolidado.Checked Then
	istr_mant.argumento[5] = '-1'
ElseIf cbx_consolidado.Checked Then
	istr_mant.argumento[5] = '-9'
Else	
	istr_mant.argumento[5] = em_embalaje.Text
End If

If cbx_proceso.Checked Then
	ll_numeroini = 1
	ll_numerofin = 99999999
Else
	ll_numeroini = long(em_procdesde.Text)
	ll_numerofin = Long(em_prochasta.Text)
End If	

If cbx_fecha.Checked Then
	ld_fechaini = date('1900-01-01')
	ld_fechafin = date(Today())
Else
	ld_fechaini = date(em_desde.Text)
	ld_fechafin = date(em_hasta.Text)
End If	

istr_info.titulo	= 'INFORME ESTADISTICO RESULTADO PROCESO'
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_packing_frigorIfico"
vinf.dw_1.SetTransObject(sqlca)

fila	=	vinf.dw_1.Retrieve(integer(istr_mant.argumento[2]),integer(istr_mant.argumento[1]),integer(istr_mant.argumento[3]),&
				ld_fechaini,ld_fechafin,ll_numeroini,ll_numerofin,istr_mant.argumento[5],Long(istr_mant.argumento[4]))
	
If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy("t_desde.text = '" + String(ld_fechaini) + "'")
	vinf.dw_1.ModIfy("t_hasta.text = '" + String(ld_fechafin) + "'")
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_packing_frigorifico
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2331
integer y = 1488
integer taborder = 190
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_4 from statictext within w_info_packing_frigorifico
integer x = 251
integer y = 440
integer width = 1902
integer height = 652
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

type st_1 from statictext within w_info_packing_frigorifico
integer x = 293
integer y = 612
integer width = 462
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

type dw_2 from datawindow within w_info_packing_frigorifico
integer x = 713
integer y = 480
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_planta	

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
	
	dw_productor.GetChild("prod_codigo", idwc_productor)
	idwc_productor.SetTransObject(sqlca)
	idwc_productor.Retrieve(Integer(data))
	dw_productor.InsertRow(0)
	
ELSE
	This.SetItem(1, "clie_codigo", gi_CodExport)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_packing_frigorifico
integer x = 293
integer y = 488
integer width = 233
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

type dw_planta from datawindow within w_info_packing_frigorifico
integer x = 713
integer y = 600
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[], ls_planta
Integer	li_Cliente, li_planta

li_Cliente	=	dw_2.Object.clie_codigo[1]

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1]	=	data
	li_planta = integer(data)
	
	select plde_nombre into :ls_planta
	from dbo.plantadesp
	where plde_codigo = :li_planta;
	
	istr_mant.Argumento[5]	=	ls_planta
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_8 from statictext within w_info_packing_frigorifico
integer x = 293
integer y = 720
integer width = 306
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
string text = "Productor"
boolean focusrectangle = false
end type

type cbx_productor from checkbox within w_info_packing_frigorifico
integer x = 1701
integer y = 732
integer width = 402
integer height = 80
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF cbx_productor.Checked = TRUE THEN
	dw_productor.Enabled  = False
	dw_productor.Reset()
	dw_productor.insertrow(0)
	istr_mant.argumento[4]	=	'-1'
ELSE
	dw_productor.Enabled  = True
	dw_productor.SetFocus()
	dw_productor.Reset()
	dw_productor.InsertRow(0)
END IF
	
end event

type dw_productor from datawindow within w_info_packing_frigorifico
integer x = 713
integer y = 720
integer width = 983
integer height = 92
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_productores_clientes"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;String	 ls_null
SetNull(ls_null)

IF ExisteProductor(Long(data)) THEN
	istr_mant.argumento[4]	=	data	
	RETURN 0
ELSE
	This.SetItem(1, "prod_codigo", Integer(ls_null))
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_3 from statictext within w_info_packing_frigorifico
integer x = 293
integer y = 848
integer width = 270
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

type dw_especie from datawindow within w_info_packing_frigorifico
integer x = 713
integer y = 832
integer width = 882
integer height = 100
integer taborder = 60
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_especies"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExisteEspecie(Integer(data)) THEN
	istr_mant.argumento[3]	=	data
ELSE
	This.SetItem(1, "espe_codigo", gi_CodEspecie)
	istr_mant.argumento[8]	=	String(gi_CodEspecie)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_embalaje from statictext within w_info_packing_frigorifico
integer x = 288
integer y = 964
integer width = 288
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
string text = "Embalaje"
boolean focusrectangle = false
end type

type cbx_embalaje from checkbox within w_info_packing_frigorifico
integer x = 1106
integer y = 948
integer width = 402
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
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
ELSE
	em_embalaje.Enabled			=	True
	cb_buscaembalaje.Enabled	=	True
END IF
end event

type em_embalaje from editmask within w_info_packing_frigorifico
integer x = 718
integer y = 940
integer width = 261
integer height = 96
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
alignment alignment = center!
textcase textcase = upper!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = stringmask!
string mask = "xxxxxxxxxx"
end type

event modified;Integer		li_cliente
String		ls_embalaje, ls_Nombre

li_cliente	=	Integer(istr_mant.argumento[2]) // Cliente
ls_embalaje	=	This.Text

SELECT	emba_nombre
	INTO	:ls_Nombre
	FROM	dbo.embalajesprod
	WHERE	clie_codigo	=	:li_cliente
	AND	emba_codigo	=	:ls_embalaje;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Embalajes")
	This.SetFocus()
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Embalaje no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	This.SetFocus()
END IF

end event

type cb_buscaembalaje from commandbutton within w_info_packing_frigorifico
integer x = 997
integer y = 944
integer width = 96
integer height = 84
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;
Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[2] // Cliente

OpenWithParm(w_busc_embalajesprod, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[2] = "" THEN
	em_embalaje.SetFocus()
ELSE
	em_embalaje.Text			=	lstr_busq.argum[2]
END IF
end event

type st_9 from statictext within w_info_packing_frigorifico
integer x = 366
integer y = 1544
integer width = 247
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
string text = "Desde"
boolean focusrectangle = false
end type

type em_procdesde from editmask within w_info_packing_frigorifico
integer x = 626
integer y = 1532
integer width = 402
integer height = 96
integer taborder = 150
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
string mask = "#####"
end type

type st_10 from statictext within w_info_packing_frigorifico
integer x = 1070
integer y = 1544
integer width = 229
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_prochasta from editmask within w_info_packing_frigorifico
integer x = 1307
integer y = 1536
integer width = 402
integer height = 96
integer taborder = 160
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
string mask = "#####"
end type

type cbx_especie from checkbox within w_info_packing_frigorifico
integer x = 1586
integer y = 844
integer width = 402
integer height = 80
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	dw_especie.Enabled  = False
	dw_especie.Reset()
	dw_especie.insertrow(0)
	istr_mant.argumento[3]	=	'-1'
ELSE
	dw_especie.Enabled  = True
	dw_especie.SetFocus()
	dw_especie.Reset()
	idwc_especie.Retrieve(1)
	dw_especie.InsertRow(0)
END IF
	
end event

type cbx_planta from checkbox within w_info_packing_frigorifico
integer x = 1701
integer y = 604
integer width = 402
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	dw_planta.Enabled  = False
	dw_planta.Reset()
	dw_planta.insertrow(0)
	istr_mant.argumento[1]	=	'-1'
ELSE
	dw_planta.Enabled  = True
	dw_planta.SetFocus()
	dw_planta.Reset()
	idwc_planta.Retrieve(1)
	dw_planta.InsertRow(0)
END IF




end event

type st_2 from statictext within w_info_packing_frigorifico
integer x = 366
integer y = 1236
integer width = 247
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
string text = "Desde"
boolean focusrectangle = false
end type

type em_desde from editmask within w_info_packing_frigorifico
integer x = 631
integer y = 1224
integer width = 402
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
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_7 from statictext within w_info_packing_frigorifico
integer x = 1070
integer y = 1236
integer width = 229
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
string text = "Hasta"
boolean focusrectangle = false
end type

type em_hasta from editmask within w_info_packing_frigorifico
integer x = 1307
integer y = 1224
integer width = 402
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
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type gb_4 from groupbox within w_info_packing_frigorifico
integer x = 283
integer y = 1124
integer width = 1810
integer height = 244
integer taborder = 200
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Proceso"
end type

type st_11 from statictext within w_info_packing_frigorifico
integer x = 251
integer y = 1096
integer width = 1902
integer height = 312
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

type gb_3 from groupbox within w_info_packing_frigorifico
integer x = 283
integer y = 1436
integer width = 1810
integer height = 244
integer taborder = 210
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nro Resultado Proceso"
end type

type st_5 from statictext within w_info_packing_frigorifico
integer x = 251
integer y = 1408
integer width = 1902
integer height = 312
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

type cbx_fecha from checkbox within w_info_packing_frigorifico
integer x = 1742
integer y = 1228
integer width = 334
integer height = 84
integer taborder = 140
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	em_desde.Enabled = False
	em_hasta.Enabled = False
ELSE
	em_desde.Enabled = True
	em_hasta.Enabled = True	
END IF
	
end event

type cbx_proceso from checkbox within w_info_packing_frigorifico
integer x = 1746
integer y = 1540
integer width = 334
integer height = 88
integer taborder = 170
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.Checked = TRUE THEN
	em_procdesde.Enabled = False
	em_prochasta.Enabled = False
	em_prochasta.Text = ''
	em_procdesde.Text = ''
ELSE
	em_procdesde.Enabled = True
	em_prochasta.Enabled = True	
END IF
	
end event

type cbx_consolidado from checkbox within w_info_packing_frigorifico
integer x = 1454
integer y = 948
integer width = 526
integer height = 80
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	em_embalaje.Enabled			=	False
	cb_buscaembalaje.Enabled	=	False
	em_embalaje.Text				=	''
	cbx_embalaje.Enabled 		=	False
ELSE
	//em_embalaje.Enabled			=	True
	//cb_buscaembalaje.Enabled	=	True
	cbx_embalaje.Enabled 		=	True	
END IF
end event

