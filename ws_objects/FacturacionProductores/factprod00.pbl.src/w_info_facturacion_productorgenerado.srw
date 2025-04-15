$PBExportHeader$w_info_facturacion_productorgenerado.srw
$PBExportComments$Informe de Facturación Productor Mensual.
forward
global type w_info_facturacion_productorgenerado from w_para_informes
end type
type st_1 from statictext within w_info_facturacion_productorgenerado
end type
type st_2 from statictext within w_info_facturacion_productorgenerado
end type
type em_fecha from editmask within w_info_facturacion_productorgenerado
end type
type st_6 from statictext within w_info_facturacion_productorgenerado
end type
type dw_planta from datawindow within w_info_facturacion_productorgenerado
end type
type st_8 from statictext within w_info_facturacion_productorgenerado
end type
type st_9 from statictext within w_info_facturacion_productorgenerado
end type
type em_cambio from editmask within w_info_facturacion_productorgenerado
end type
type em_poriva from editmask within w_info_facturacion_productorgenerado
end type
type st_7 from statictext within w_info_facturacion_productorgenerado
end type
type cbx_consolidado from checkbox within w_info_facturacion_productorgenerado
end type
type st_3 from statictext within w_info_facturacion_productorgenerado
end type
type dw_zona from datawindow within w_info_facturacion_productorgenerado
end type
type cbx_todos from checkbox within w_info_facturacion_productorgenerado
end type
type st_10 from statictext within w_info_facturacion_productorgenerado
end type
type cbx_consovariedades from checkbox within w_info_facturacion_productorgenerado
end type
type cbx_consocalibres from checkbox within w_info_facturacion_productorgenerado
end type
type cbx_consoembalajes from checkbox within w_info_facturacion_productorgenerado
end type
type cbx_1 from checkbox within w_info_facturacion_productorgenerado
end type
type st_5 from statictext within w_info_facturacion_productorgenerado
end type
type st_11 from statictext within w_info_facturacion_productorgenerado
end type
type cbx_guia from checkbox within w_info_facturacion_productorgenerado
end type
type rb_produccion from radiobutton within w_info_facturacion_productorgenerado
end type
type rb_granel from radiobutton within w_info_facturacion_productorgenerado
end type
type rb_compara from radiobutton within w_info_facturacion_productorgenerado
end type
type gb_6 from groupbox within w_info_facturacion_productorgenerado
end type
type st_12 from statictext within w_info_facturacion_productorgenerado
end type
type em_mdesde from editmask within w_info_facturacion_productorgenerado
end type
type em_mhasta from editmask within w_info_facturacion_productorgenerado
end type
type st_4 from statictext within w_info_facturacion_productorgenerado
end type
type st_13 from statictext within w_info_facturacion_productorgenerado
end type
type st_14 from statictext within w_info_facturacion_productorgenerado
end type
type uo_selproductor from uo_seleccion_productor within w_info_facturacion_productorgenerado
end type
type st_15 from statictext within w_info_facturacion_productorgenerado
end type
type uo_selespecie from uo_seleccion_especie within w_info_facturacion_productorgenerado
end type
type uo_selcliente from uo_seleccion_clientesprod within w_info_facturacion_productorgenerado
end type
type em_numero from editmask within w_info_facturacion_productorgenerado
end type
type st_16 from statictext within w_info_facturacion_productorgenerado
end type
type cb_proforma from commandbutton within w_info_facturacion_productorgenerado
end type
end forward

global type w_info_facturacion_productorgenerado from w_para_informes
integer x = 14
integer y = 32
integer width = 2930
integer height = 2236
string title = "Facturación Mensual de Productores"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
st_2 st_2
em_fecha em_fecha
st_6 st_6
dw_planta dw_planta
st_8 st_8
st_9 st_9
em_cambio em_cambio
em_poriva em_poriva
st_7 st_7
cbx_consolidado cbx_consolidado
st_3 st_3
dw_zona dw_zona
cbx_todos cbx_todos
st_10 st_10
cbx_consovariedades cbx_consovariedades
cbx_consocalibres cbx_consocalibres
cbx_consoembalajes cbx_consoembalajes
cbx_1 cbx_1
st_5 st_5
st_11 st_11
cbx_guia cbx_guia
rb_produccion rb_produccion
rb_granel rb_granel
rb_compara rb_compara
gb_6 gb_6
st_12 st_12
em_mdesde em_mdesde
em_mhasta em_mhasta
st_4 st_4
st_13 st_13
st_14 st_14
uo_selproductor uo_selproductor
st_15 st_15
uo_selespecie uo_selespecie
uo_selcliente uo_selcliente
em_numero em_numero
st_16 st_16
cb_proforma cb_proforma
end type
global w_info_facturacion_productorgenerado w_info_facturacion_productorgenerado

type variables
str_busqueda istr_busq
str_mant istr_mant

DataWindowChild	idwc_zona, idwc_planta
Long		il_cont
end variables

forward prototypes
public subroutine wf_cargaproforma (integer cliente, long planta, date periodo)
public subroutine wf_existefactura ()
public subroutine wf_buscaproforma (integer cliente, long planta, date periodo)
end prototypes

public subroutine wf_cargaproforma (integer cliente, long planta, date periodo);Integer	li_Cantidad, li_Secuencia
Date		ld_Desde, ld_Hasta


 Select Count(plde_codigo)
	Into :li_Cantidad
	  From dbo.facturprodenca
	 Where clie_codigo= :Cliente
		  And :Planta in (-9,plde_codigo)
		  And Datediff(mm, :Periodo, faen_fechaf) = 0
		  And faen_estado = 1
	Using SQLCA;

IF SQLCA.SQLCode = -1 THEN
	MessageBox("Error", "No se pudo efectur revision.", StopSign!)
	em_numero.Text = ''
	em_mDesde.Text = ''
	em_mHasta.Text = ''
	Return
Else
	If li_Cantidad = 0 Then
		MessageBox("Atencion", "No existen Facturas proforma para periodo seleccionado.", Exclamation!)
		em_numero.Text = ''
		em_mDesde.Text = ''
		em_mHasta.Text = ''
		Return
	ElseIf li_Cantidad = 1 Then
		
		 Select faen_secuen, faen_fecini, faen_fecter
			Into :li_Secuencia, :ld_Desde, :ld_Hasta
			  From dbo.facturprodenca
			 Where clie_codigo= :Cliente
				  And :Planta in (-9,plde_codigo)
				  And Datediff(mm, :Periodo, faen_fechaf) = 0
				   And faen_estado = 1
			Using SQLCA;
			
		If SQLCA.SQLCode = -1 THEN
			MessageBox("Error", "No se pudo conectar a la base de datos.", StopSign!)
			Return
		Else
			em_numero.Text = String(li_Secuencia)
			em_mDesde.Text = String(ld_Desde, 'dd/mm/yyyy')
			em_mHasta.Text = String(ld_Hasta, 'dd/mm/yyyy')
		End If
	Else
		wf_BuscaProforma(Cliente, Planta, Periodo)
		
	End If
END IF
end subroutine

public subroutine wf_existefactura ();Integer	li_cliente, li_planta
Date		ld_fecha
Decimal  ld_cambio, ld_iva

li_Cliente	=	uo_SelCliente.Codigo
li_Planta	=	Integer(istr_mant.argumento[2])
ld_fecha		=	Date(istr_mant.argumento[3])
il_cont		= 	0

SELECT DISTINCT faen_cambio, faen_poriva
INTO	:ld_cambio, :ld_iva
FROM dbo.facturprodenca
WHERE clie_codigo=:li_cliente
AND   :li_planta in (-9,plde_codigo)
AND   faen_fechaf=:ld_fecha;

IF sqlca.SQLCode = -1 THEN
	MessageBox("Atención", "Mes de Proceso Tiene mas de 1 Valor de Tipo de Cambio Por favor Revise los Datos.")
	il_cont = 1
	Return
END IF

IF (isnull(ld_cambio) OR ld_cambio=0 ) AND (isnull(ld_iva) OR ld_iva=0) THEN
	SELECT DISTINCT faen_cambio, faen_poriva
	INTO	:ld_cambio, :ld_iva
	FROM dbo.facturprodenca_granel
	WHERE clie_codigo=:li_cliente
	AND   :li_planta in (-9,plde_codigo)
	AND   faen_fechaf=:ld_fecha;
	
	IF sqlca.SQLCode = -1 THEN
		MessageBox("Atención", "Mes de Proceso Tiene mas de 1 Valor de Tipo de Cambio Por favor Revise los Datos.")
		il_cont = 1
		Return
	END IF
END IF

em_poriva.Text	=	String(ld_iva)
em_cambio.Text	=	String(ld_cambio)

end subroutine

public subroutine wf_buscaproforma (integer cliente, long planta, date periodo);str_busqueda	lstr_busq
str_Mant			lstr_Mant

lstr_Busq.Argum[1]	=	String(Cliente)
lstr_Busq.Argum[2]	=	String(Planta)
lstr_Busq.Argum[3]	=	String(Periodo, 'dd/mm/yyyy')
lstr_Busq.Argum[4]	=	'1'

OpenWithParm(w_busc_proforma,lstr_busq)

lstr_Mant	= Message.PowerObjectParm

If UpperBound(lstr_Mant.Argumento) > 0 Then
	em_numero.Text	= lstr_Mant.Argumento[1]
	em_mDesde.Text	= lstr_Mant.Argumento[2]
	em_mHasta.Text	= lstr_Mant.Argumento[3]
End If

end subroutine

on w_info_facturacion_productorgenerado.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.em_fecha=create em_fecha
this.st_6=create st_6
this.dw_planta=create dw_planta
this.st_8=create st_8
this.st_9=create st_9
this.em_cambio=create em_cambio
this.em_poriva=create em_poriva
this.st_7=create st_7
this.cbx_consolidado=create cbx_consolidado
this.st_3=create st_3
this.dw_zona=create dw_zona
this.cbx_todos=create cbx_todos
this.st_10=create st_10
this.cbx_consovariedades=create cbx_consovariedades
this.cbx_consocalibres=create cbx_consocalibres
this.cbx_consoembalajes=create cbx_consoembalajes
this.cbx_1=create cbx_1
this.st_5=create st_5
this.st_11=create st_11
this.cbx_guia=create cbx_guia
this.rb_produccion=create rb_produccion
this.rb_granel=create rb_granel
this.rb_compara=create rb_compara
this.gb_6=create gb_6
this.st_12=create st_12
this.em_mdesde=create em_mdesde
this.em_mhasta=create em_mhasta
this.st_4=create st_4
this.st_13=create st_13
this.st_14=create st_14
this.uo_selproductor=create uo_selproductor
this.st_15=create st_15
this.uo_selespecie=create uo_selespecie
this.uo_selcliente=create uo_selcliente
this.em_numero=create em_numero
this.st_16=create st_16
this.cb_proforma=create cb_proforma
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.em_fecha
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.dw_planta
this.Control[iCurrent+6]=this.st_8
this.Control[iCurrent+7]=this.st_9
this.Control[iCurrent+8]=this.em_cambio
this.Control[iCurrent+9]=this.em_poriva
this.Control[iCurrent+10]=this.st_7
this.Control[iCurrent+11]=this.cbx_consolidado
this.Control[iCurrent+12]=this.st_3
this.Control[iCurrent+13]=this.dw_zona
this.Control[iCurrent+14]=this.cbx_todos
this.Control[iCurrent+15]=this.st_10
this.Control[iCurrent+16]=this.cbx_consovariedades
this.Control[iCurrent+17]=this.cbx_consocalibres
this.Control[iCurrent+18]=this.cbx_consoembalajes
this.Control[iCurrent+19]=this.cbx_1
this.Control[iCurrent+20]=this.st_5
this.Control[iCurrent+21]=this.st_11
this.Control[iCurrent+22]=this.cbx_guia
this.Control[iCurrent+23]=this.rb_produccion
this.Control[iCurrent+24]=this.rb_granel
this.Control[iCurrent+25]=this.rb_compara
this.Control[iCurrent+26]=this.gb_6
this.Control[iCurrent+27]=this.st_12
this.Control[iCurrent+28]=this.em_mdesde
this.Control[iCurrent+29]=this.em_mhasta
this.Control[iCurrent+30]=this.st_4
this.Control[iCurrent+31]=this.st_13
this.Control[iCurrent+32]=this.st_14
this.Control[iCurrent+33]=this.uo_selproductor
this.Control[iCurrent+34]=this.st_15
this.Control[iCurrent+35]=this.uo_selespecie
this.Control[iCurrent+36]=this.uo_selcliente
this.Control[iCurrent+37]=this.em_numero
this.Control[iCurrent+38]=this.st_16
this.Control[iCurrent+39]=this.cb_proforma
end on

on w_info_facturacion_productorgenerado.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_fecha)
destroy(this.st_6)
destroy(this.dw_planta)
destroy(this.st_8)
destroy(this.st_9)
destroy(this.em_cambio)
destroy(this.em_poriva)
destroy(this.st_7)
destroy(this.cbx_consolidado)
destroy(this.st_3)
destroy(this.dw_zona)
destroy(this.cbx_todos)
destroy(this.st_10)
destroy(this.cbx_consovariedades)
destroy(this.cbx_consocalibres)
destroy(this.cbx_consoembalajes)
destroy(this.cbx_1)
destroy(this.st_5)
destroy(this.st_11)
destroy(this.cbx_guia)
destroy(this.rb_produccion)
destroy(this.rb_granel)
destroy(this.rb_compara)
destroy(this.gb_6)
destroy(this.st_12)
destroy(this.em_mdesde)
destroy(this.em_mhasta)
destroy(this.st_4)
destroy(this.st_13)
destroy(this.st_14)
destroy(this.uo_selproductor)
destroy(this.st_15)
destroy(this.uo_selespecie)
destroy(this.uo_selcliente)
destroy(this.em_numero)
destroy(this.st_16)
destroy(this.cb_proforma)
end on

event open;call super::open;Boolean lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelProductor.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelProductor.Seleccion(True, True)
	uo_SelProductor.Filtra(-1)
	uo_SelCliente.Inicia(gi_CodExport)

	dw_zona.GetChild("zona_codigo", idwc_zona)
	idwc_zona.SetTransObject(sqlca)
	idwc_zona.Retrieve()
	dw_zona.InsertRow(1)
	dw_zona.SetItem(1,"zona_codigo", 200)
	
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(sqlca)
	idwc_planta.Retrieve(1, 200)
	dw_planta.InsertRow(1)
	
	em_Fecha.Text				=	String(Today())
	
	istr_mant.argumento[2]	= 	String(gi_CodPlanta)
	istr_mant.argumento[3]	= 	'01/' + em_Fecha.Text
	istr_mant.argumento[10] =  '200'
	
	wf_Existefactura()
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_facturacion_productorgenerado
end type

type st_computador from w_para_informes`st_computador within w_info_facturacion_productorgenerado
end type

type st_usuario from w_para_informes`st_usuario within w_info_facturacion_productorgenerado
end type

type st_temporada from w_para_informes`st_temporada within w_info_facturacion_productorgenerado
end type

type p_logo from w_para_informes`p_logo within w_info_facturacion_productorgenerado
end type

type st_titulo from w_para_informes`st_titulo within w_info_facturacion_productorgenerado
integer width = 2112
string text = "Informe de Facturación Mensual de Productores"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_facturacion_productorgenerado
integer x = 2473
integer y = 1532
integer taborder = 120
string powertiptext = "Imprime Informe"
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	ll_Filas, li_Planta, li_Zona, li_Consolidado, li_Variedades, &
			li_Calibres, li_Embalajes, li_Guia, li_Secuencia
Date		ld_MesProceso, ld_Fdesde, ld_Fhasta
Decimal{2}	ld_PorIVA, ld_ValCambio

IF il_cont > 1 THEN
	MessageBox( "Atención", "Revise Datos de De valores de Tipo Cambio.", StopSign!, Ok!)
	Return
END IF					 

DataWindowChild	ldwc_planta
istr_info.titulo	= 'FACTURACION MENSUAL DE PRODUCTORES'
OpenWithParm(vinf, istr_info)


ld_Fdesde	=	Date(em_Mdesde.Text)
ld_FHasta	=	Date(em_Mhasta.Text)	

li_Secuencia = Integer(em_numero.Text)
If IsNull(li_Secuencia) Or li_Secuencia = 0 Then
	MessageBox('Atencio', 'Debe seleccionar una factura a emitir.', Information!, Ok!)
	Return
End If

li_Guia	=	0
IF cbx_guia.CheCked THEN
	li_Guia	=	1
END IF

IF cbx_1.checked THEN
	vinf.dw_1.DataObject = "dw_info_facturacion_productorgeneradodet"
ELSEIF rb_compara.Checked THEN
	vinf.dw_1.DataObject = "dw_info_facturacion_productorgenerado_grnel" 
ELSEIF rb_produccion.Checked THEN
	vinf.dw_1.DataObject = "dw_info_facturacion_productorgenerado" 
ELSE
	vinf.dw_1.DataObject = "dw_info_facturacion_productorgeneradoproceso" 
END IF

IF cbx_todos.checked = True THEN
	li_zona = -1
ElSE
	li_zona = Integer(istr_mant.argumento[10])
END IF	

IF cbx_consolidado.checked = True THEN 
	li_planta = -9
	wf_ExisteFactura()
ELSE
	li_Planta		=	Integer(istr_mant.argumento[2])
	wf_ExisteFactura()
END IF

IF cbx_ConsoVariedades.Checked THEN
	li_Variedades	=	-9
ELSE
	li_Variedades	=	0
END IF

IF cbx_ConsoCalibres.Checked THEN
	li_Calibres	=	-9
ELSE
	li_Calibres	=	0
END IF

IF cbx_ConsoEmbalajes.Checked THEN
	li_Embalajes	=	-9
ELSE
	li_Embalajes	=	0
END IF

ld_MesProceso 	= 	Date("01/" + em_Fecha.Text)
IF il_cont > 0 THEN Return

ld_PorIVA		=	Dec(em_poriva.Text)
ld_ValCambio	=	Dec(em_Cambio.Text)

vinf.dw_1.GetChild("plde_codigo", ldwc_planta)
ldwc_planta.SetTransObject(sqlca)
ldwc_planta.Retrieve()

vinf.dw_1.SetTransObject(sqlca)

ll_Filas	=	vinf.dw_1.Retrieve(uo_SelCliente.Codigo, li_Planta, ld_MesProceso, uo_SelProductor.Codigo,li_zona, ld_ValCambio, ld_PorIVA,&
										li_Variedades,li_Calibres,li_Embalajes,li_Guia, ld_Fdesde,ld_Fhasta,uo_SelEspecie.Codigo, li_Secuencia)
IF ll_Filas = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF ll_Filas = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)				
end event

type pb_salir from w_para_informes`pb_salir within w_info_facturacion_productorgenerado
integer x = 2473
integer y = 1832
integer taborder = 130
string powertiptext = "Salir de la Ventana"
end type

type st_1 from statictext within w_info_facturacion_productorgenerado
integer x = 343
integer y = 672
integer width = 462
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_info_facturacion_productorgenerado
integer x = 343
integer y = 784
integer width = 485
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Mes de Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_info_facturacion_productorgenerado
integer x = 850
integer y = 772
integer width = 393
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "mm/yyyy"
end type

event modified;istr_mant.argumento[3]	=	'01/' + This.Text
wf_ExisteFactura()
wf_CargaProforma(uo_SelCliente.Codigo, Long(istr_mant.argumento[2]), Date('01/' + This.Text))

end event

type st_6 from statictext within w_info_facturacion_productorgenerado
integer x = 343
integer y = 472
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_info_facturacion_productorgenerado
integer x = 850
integer y = 672
integer width = 969
integer height = 92
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo_zona"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String	ls_Columna[]
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.argumento[1])

IF ExistePlanta(li_Cliente, Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2]	=	data
ELSE
	This.SetItem(1, "plde_codigo", gi_CodPlanta)
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_8 from statictext within w_info_facturacion_productorgenerado
integer x = 343
integer y = 904
integer width = 411
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Valor Cambio"
boolean focusrectangle = false
end type

type st_9 from statictext within w_info_facturacion_productorgenerado
integer x = 1193
integer y = 904
integer width = 270
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "%  I.V.A."
boolean focusrectangle = false
end type

type em_cambio from editmask within w_info_facturacion_productorgenerado
integer x = 850
integer y = 892
integer width = 315
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "##,###.##"
end type

type em_poriva from editmask within w_info_facturacion_productorgenerado
integer x = 1445
integer y = 892
integer width = 238
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "#0.00"
end type

type st_7 from statictext within w_info_facturacion_productorgenerado
integer x = 247
integer y = 1276
integer width = 2112
integer height = 256
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

type cbx_consolidado from checkbox within w_info_facturacion_productorgenerado
integer x = 1842
integer y = 672
integer width = 475
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolidado"
end type

event clicked;
IF cbx_consolidado.checked = True THEN
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(166,180,210)
	dw_planta.Enabled		=	False
	dw_planta.Reset()
	idwc_planta.SetTransObject(sqlca)
	idwc_planta.Retrieve(gi_CodExport,1,0)
	dw_planta.InsertRow(1)
	istr_mant.argumento[2]	= 	'-9'
ELSE
	dw_planta.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
	dw_planta.Enabled = True
END IF
end event

type st_3 from statictext within w_info_facturacion_productorgenerado
integer x = 343
integer y = 572
integer width = 247
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Oficina"
boolean focusrectangle = false
end type

type dw_zona from datawindow within w_info_facturacion_productorgenerado
integer x = 850
integer y = 572
integer width = 846
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[10]	=	string(data)


idwc_planta.SetTransObject(sqlca)
dw_planta.reset()
idwc_planta.Retrieve(1,Integer(data))
dw_planta.InsertRow(0)
end event

type cbx_todos from checkbox within w_info_facturacion_productorgenerado
boolean visible = false
integer x = 1842
integer y = 572
integer width = 311
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Todas"
end type

event clicked;IF This.CheCked THEN
	dw_zona.Enabled = False
	dw_zona.SetTabOrder("zona_codigo",0)
	dw_zona.Modify("zona_codigo.BackGround.Color = " + String(RGB(166,180,210)))
	dw_zona.Reset()
	idwc_zona.SetTransObject(sqlca)
	idwc_zona.Retrieve(gi_CodExport)
	dw_zona.InsertRow(1)
	idwc_planta.Retrieve(gi_CodExport,1,0)
ELSE
	dw_zona.Enabled = True
	dw_zona.SetTabOrder("zona_codigo", 10)
	dw_zona.Modify("zona_codigo.BackGround.Color = " + String(rgb(255,255,255)))
	//dw_zona.SetItem(1,"zona_codigo",2)
END IF
end event

type st_10 from statictext within w_info_facturacion_productorgenerado
integer x = 777
integer y = 1532
integer width = 1051
integer height = 288
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

type cbx_consovariedades from checkbox within w_info_facturacion_productorgenerado
integer x = 901
integer y = 1568
integer width = 823
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolida Variedades"
end type

type cbx_consocalibres from checkbox within w_info_facturacion_productorgenerado
integer x = 901
integer y = 1648
integer width = 823
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolida Calibres"
end type

type cbx_consoembalajes from checkbox within w_info_facturacion_productorgenerado
integer x = 901
integer y = 1724
integer width = 823
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Consolida Embalajes"
end type

type cbx_1 from checkbox within w_info_facturacion_productorgenerado
integer x = 338
integer y = 1636
integer width = 375
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Resumen"
end type

type st_5 from statictext within w_info_facturacion_productorgenerado
integer x = 247
integer y = 1532
integer width = 530
integer height = 288
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

type st_11 from statictext within w_info_facturacion_productorgenerado
integer x = 1829
integer y = 1532
integer width = 530
integer height = 288
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

type cbx_guia from checkbox within w_info_facturacion_productorgenerado
integer x = 1947
integer y = 1636
integer width = 302
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Guías"
end type

type rb_produccion from radiobutton within w_info_facturacion_productorgenerado
integer x = 530
integer y = 1908
integer width = 471
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Producción"
boolean checked = true
end type

type rb_granel from radiobutton within w_info_facturacion_productorgenerado
integer x = 1056
integer y = 1908
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Procesos"
end type

type rb_compara from radiobutton within w_info_facturacion_productorgenerado
integer x = 1536
integer y = 1908
integer width = 507
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Comparativo"
end type

type gb_6 from groupbox within w_info_facturacion_productorgenerado
integer x = 315
integer y = 1832
integer width = 1934
integer height = 184
integer taborder = 110
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Informe"
end type

type st_12 from statictext within w_info_facturacion_productorgenerado
integer x = 247
integer y = 1820
integer width = 2112
integer height = 236
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

type em_mdesde from editmask within w_info_facturacion_productorgenerado
integer x = 1353
integer y = 772
integer width = 352
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;Date	ld_MesFactur, ld_Fecha, ld_nulo

SetNull(ld_nulo)

ld_MesFactur	=	Date('01/'+em_fecha.Text)
ld_Fecha			=	Date(em_Mdesde.Text)

IF Year(ld_Fecha) <> Year(ld_MesFactur) OR Month(ld_Fecha) <> Month(ld_MesFactur) THEN
	em_Mdesde.Text	=	String(ld_nulo)
	em_Mdesde.SetFocus()
END IF
end event

type em_mhasta from editmask within w_info_facturacion_productorgenerado
integer x = 1783
integer y = 772
integer width = 352
integer height = 92
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

event modified;Date	ld_MesFactur, ld_Fecha, ld_nulo

SetNull(ld_nulo)

ld_MesFactur	=	Date('01/'+em_fecha.Text)
ld_Fecha			=	Date(em_Mhasta.Text)

IF Year(ld_Fecha) <> Year(ld_MesFactur) OR Month(ld_Fecha) <> Month(ld_MesFactur) THEN
	
	em_Mhasta.Text	=	String(ld_nulo)
	em_Mhasta.SetFocus()
	
END IF
end event

type st_4 from statictext within w_info_facturacion_productorgenerado
integer x = 247
integer y = 440
integer width = 2112
integer height = 596
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

type st_13 from statictext within w_info_facturacion_productorgenerado
integer x = 247
integer y = 1032
integer width = 2112
integer height = 240
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

type st_14 from statictext within w_info_facturacion_productorgenerado
integer x = 343
integer y = 1140
integer width = 503
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selproductor from uo_seleccion_productor within w_info_facturacion_productorgenerado
event destroy ( )
integer x = 850
integer y = 1308
integer taborder = 80
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

type st_15 from statictext within w_info_facturacion_productorgenerado
integer x = 343
integer y = 1388
integer width = 503
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_facturacion_productorgenerado
event destroy ( )
integer x = 850
integer y = 1056
integer taborder = 80
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selcliente from uo_seleccion_clientesprod within w_info_facturacion_productorgenerado
event destroy ( )
integer x = 850
integer y = 460
integer height = 88
integer taborder = 40
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type em_numero from editmask within w_info_facturacion_productorgenerado
integer x = 1902
integer y = 892
integer width = 233
integer height = 92
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16777215
boolean enabled = false
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "###"
end type

type st_16 from statictext within w_info_facturacion_productorgenerado
integer x = 1742
integer y = 904
integer width = 165
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Sec."
boolean focusrectangle = false
end type

type cb_proforma from commandbutton within w_info_facturacion_productorgenerado
integer x = 2153
integer y = 892
integer width = 123
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;wf_BuscaProforma(uo_SelCliente.Codigo, Long(istr_mant.argumento[2]), Date('01/' + em_fecha.Text))
end event

