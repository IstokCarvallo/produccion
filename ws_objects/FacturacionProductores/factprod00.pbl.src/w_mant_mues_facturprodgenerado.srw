$PBExportHeader$w_mant_mues_facturprodgenerado.srw
$PBExportComments$Mantención Valores de Facturación por Productor.
forward
global type w_mant_mues_facturprodgenerado from w_mant_tabla
end type
type st_3 from statictext within w_mant_mues_facturprodgenerado
end type
type em_produc from editmask within w_mant_mues_facturprodgenerado
end type
type cb_2 from uo_buscar within w_mant_mues_facturprodgenerado
end type
type sle_nompro from singlelineedit within w_mant_mues_facturprodgenerado
end type
type sle_nomzona from singlelineedit within w_mant_mues_facturprodgenerado
end type
type st_5 from statictext within w_mant_mues_facturprodgenerado
end type
type st_1 from statictext within w_mant_mues_facturprodgenerado
end type
type st_2 from statictext within w_mant_mues_facturprodgenerado
end type
type em_fecha from editmask within w_mant_mues_facturprodgenerado
end type
type em_cambio from editmask within w_mant_mues_facturprodgenerado
end type
type em_poriva from editmask within w_mant_mues_facturprodgenerado
end type
type st_4 from statictext within w_mant_mues_facturprodgenerado
end type
type st_6 from statictext within w_mant_mues_facturprodgenerado
end type
type st_7 from statictext within w_mant_mues_facturprodgenerado
end type
type cbx_consovariedades from checkbox within w_mant_mues_facturprodgenerado
end type
type cbx_consocalibres from checkbox within w_mant_mues_facturprodgenerado
end type
type cbx_consoembalajes from checkbox within w_mant_mues_facturprodgenerado
end type
type em_mdesde from editmask within w_mant_mues_facturprodgenerado
end type
type em_mhasta from editmask within w_mant_mues_facturprodgenerado
end type
type st_16 from statictext within w_mant_mues_facturprodgenerado
end type
type em_numero from editmask within w_mant_mues_facturprodgenerado
end type
type cb_proforma from commandbutton within w_mant_mues_facturprodgenerado
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_facturprodgenerado
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_facturprodgenerado
end type
type str_anexos from structure within w_mant_mues_facturprodgenerado
end type
end forward

type str_anexos from structure
	string		titulo
	string		nominf
	string		nomdw
end type

global type w_mant_mues_facturprodgenerado from w_mant_tabla
integer width = 3977
integer height = 1912
string title = "MANTENCION DE FACTURACION POR PRODUCTOR"
st_3 st_3
em_produc em_produc
cb_2 cb_2
sle_nompro sle_nompro
sle_nomzona sle_nomzona
st_5 st_5
st_1 st_1
st_2 st_2
em_fecha em_fecha
em_cambio em_cambio
em_poriva em_poriva
st_4 st_4
st_6 st_6
st_7 st_7
cbx_consovariedades cbx_consovariedades
cbx_consocalibres cbx_consocalibres
cbx_consoembalajes cbx_consoembalajes
em_mdesde em_mdesde
em_mhasta em_mhasta
st_16 st_16
em_numero em_numero
cb_proforma cb_proforma
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
end type
global w_mant_mues_facturprodgenerado w_mant_mues_facturprodgenerado

type variables
w_mant_deta_facturprodgenerado	iw_mantencion

DataWindowChild				idwc_especie, idwc_planta
end variables

forward prototypes
public function boolean noexistecliente (integer ai_cliente)
public subroutine wf_cargaproforma (integer cliente, long planta, date periodo)
public subroutine wf_buscaproforma (integer cliente, long planta, date periodo)
public subroutine wf_existefactura ()
end prototypes

public function boolean noexistecliente (integer ai_cliente);String		ls_Nombre

SELECT	clie_nombre
	INTO	:ls_Nombre
	FROM	dbo.clientesprod
	WHERE	clie_codigo	=	:ai_Cliente ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Clientes Producción")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
					
	RETURN True
ELSE
	RETURN False
END IF
end function

public subroutine wf_cargaproforma (integer cliente, long planta, date periodo);Integer	li_Cantidad, li_Secuencia
Date		ld_Desde, ld_Hasta


 Select Count(plde_codigo)
	Into :li_Cantidad
	  From dbo.facturprodenca
	 Where clie_codigo= :Cliente
		  And :Planta in (-9,plde_codigo)
		  And Datediff(mm, :Periodo, faen_fechaf) = 0
		  And faen_estado = 0
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
				   And faen_estado = 0
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

wf_ExisteFactura()
end subroutine

public subroutine wf_buscaproforma (integer cliente, long planta, date periodo);str_busqueda	lstr_busq
str_Mant			lstr_Mant

lstr_Busq.Argum[1]	=	String(Cliente)
lstr_Busq.Argum[2]	=	String(Planta)
lstr_Busq.Argum[3]	=	String(Periodo, 'dd/mm/yyyy')
lstr_Busq.Argum[4]	=	'0'

OpenWithParm(w_busc_proforma,lstr_busq)

lstr_Mant	= Message.PowerObjectParm

If UpperBound(lstr_Mant.Argumento) > 0 Then
	em_numero.Text	= lstr_Mant.Argumento[1]
	em_mDesde.Text	= lstr_Mant.Argumento[2]
	em_mHasta.Text	= lstr_Mant.Argumento[3]
End If

end subroutine

public subroutine wf_existefactura ();Integer	li_numero, li_zona
Date		ld_fecha
Decimal  ll_cambio, ll_poriva
Long		ll_prod

li_zona		=	Integer(istr_mant.argumento[9])
ll_prod		=	Long(istr_mant.argumento[4])
ld_fecha		=	Date(istr_mant.argumento[3])

SELECT faen_cambio, faen_poriva
INTO	:ll_cambio, :ll_poriva
FROM dbo.facturprodenca
WHERE clie_codigo=:uo_SelCliente.Codigo
AND   zona_codigo=:li_zona
AND   prod_codigo=:ll_prod
AND   plde_codigo=:uo_SelPlanta.Codigo
AND   faen_fechaf=:ld_fecha
And	faen_secuen = :li_numero;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Facturprodenca")
END IF

em_poriva.Text	=	String(ll_poriva)
em_cambio.Text	=	String(ll_cambio)

end subroutine

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
Integer	li_Variedades, li_Calibres, li_Embalajes

istr_info.titulo	= "Valores de Facturación por Productor"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_facturacion_productorgenerado"

vinf.dw_1.SetTransObject(sqlca)

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

fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo,uo_SelPlanta.Codigo, Date(istr_mant.argumento[3]),Long(istr_mant.argumento[4]),&
								  Integer(istr_mant.argumento[5]),Dec(em_cambio.Text),Dec(em_poriva.Text), li_Variedades,li_Calibres,li_Embalajes)
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo,uo_SelPlanta.Codigo,Date(istr_mant.argumento[3]),Long(istr_mant.argumento[4]), Integer(em_Numero.Text))
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= 	True
		pb_eliminar.Enabled	= 	True
		pb_grabar.Enabled	= 	True
		pb_insertar.Enabled	=	True
		
		wf_ExisteFactura()
		
	ELSE
		pb_insertar.Enabled	=	False
		pb_nuevo.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN
	Close(This)
END IF
end event

on w_mant_mues_facturprodgenerado.create
int iCurrent
call super::create
this.st_3=create st_3
this.em_produc=create em_produc
this.cb_2=create cb_2
this.sle_nompro=create sle_nompro
this.sle_nomzona=create sle_nomzona
this.st_5=create st_5
this.st_1=create st_1
this.st_2=create st_2
this.em_fecha=create em_fecha
this.em_cambio=create em_cambio
this.em_poriva=create em_poriva
this.st_4=create st_4
this.st_6=create st_6
this.st_7=create st_7
this.cbx_consovariedades=create cbx_consovariedades
this.cbx_consocalibres=create cbx_consocalibres
this.cbx_consoembalajes=create cbx_consoembalajes
this.em_mdesde=create em_mdesde
this.em_mhasta=create em_mhasta
this.st_16=create st_16
this.em_numero=create em_numero
this.cb_proforma=create cb_proforma
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.em_produc
this.Control[iCurrent+3]=this.cb_2
this.Control[iCurrent+4]=this.sle_nompro
this.Control[iCurrent+5]=this.sle_nomzona
this.Control[iCurrent+6]=this.st_5
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.st_2
this.Control[iCurrent+9]=this.em_fecha
this.Control[iCurrent+10]=this.em_cambio
this.Control[iCurrent+11]=this.em_poriva
this.Control[iCurrent+12]=this.st_4
this.Control[iCurrent+13]=this.st_6
this.Control[iCurrent+14]=this.st_7
this.Control[iCurrent+15]=this.cbx_consovariedades
this.Control[iCurrent+16]=this.cbx_consocalibres
this.Control[iCurrent+17]=this.cbx_consoembalajes
this.Control[iCurrent+18]=this.em_mdesde
this.Control[iCurrent+19]=this.em_mhasta
this.Control[iCurrent+20]=this.st_16
this.Control[iCurrent+21]=this.em_numero
this.Control[iCurrent+22]=this.cb_proforma
this.Control[iCurrent+23]=this.uo_selcliente
this.Control[iCurrent+24]=this.uo_selplanta
end on

on w_mant_mues_facturprodgenerado.destroy
call super::destroy
destroy(this.st_3)
destroy(this.em_produc)
destroy(this.cb_2)
destroy(this.sle_nompro)
destroy(this.sle_nomzona)
destroy(this.st_5)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.em_fecha)
destroy(this.em_cambio)
destroy(this.em_poriva)
destroy(this.st_4)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.cbx_consovariedades)
destroy(this.cbx_consocalibres)
destroy(this.cbx_consoembalajes)
destroy(this.em_mdesde)
destroy(this.em_mhasta)
destroy(this.st_16)
destroy(this.em_numero)
destroy(this.cb_proforma)
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
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
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Inicia(gi_CodPlanta)
	
	em_Fecha.Text				=	String(Today())
	istr_mant.argumento[3]	= 	'01/'+em_Fecha.Text
	istr_mant.argumento[5]	=	String(gi_CodEspecie)
	istr_mant.argumento[6]	=	String(gi_CodVariedad)
	
	buscar	=	"Código Especie:Nespe_codigo,Código Variedad:Nvari_codigo,Nombre Variedad:Svari_nombre,Calibre:Svaca_calibr"
	ordenar	=	"Código Especie:espe_codigo,Código Variedad:vari_codigo,Nombre Variedad:vari_nombre,Calibre:vaca_calibr"
End If
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	
	istr_mant.argumento[1]	=	String(uo_SelCliente.Codigo)
	istr_mant.argumento[2]	=	String(uo_SelPlanta.Codigo)
	
	istr_mant.argumento[5]	=	String(dw_1.Object.espe_codigo[il_fila])
	istr_mant.argumento[6]	=	String(dw_1.Object.vari_codigo[il_fila])
   	istr_mant.argumento[15] = '1'
	
	istr_mant.agrega			= 	False
	istr_mant.borra			= 	False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_listo;IF dw_1.RowCount() > 0 THEN
	pb_Imprimir.Enabled	=	True
	
	IF istr_mant.Solo_Consulta THEN
		pb_Eliminar.Enabled	=	False
		pb_Grabar.Enabled		=	False
		pb_Insertar.Enabled	=	False
	ELSE
		pb_Eliminar.Enabled	=	True
		pb_Grabar.Enabled		=	True
		pb_Insertar.Enabled	=	True
	END IF
ELSE
	IF istr_mant.Solo_Consulta THEN
		pb_Insertar.Enabled	=	False
	ELSE
		pb_Insertar.Enabled	=	False
	END IF
END IF

w_main.SetMicroHelp("Listo")

SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila, ll_filas, ll_prod
Integer	li_secuencia, li_zona
Date		ld_fecha

li_zona	   =	Integer(istr_mant.argumento[9])
ll_prod     =  Long(istr_mant.argumento[4])
ld_fecha		=	Date(istr_mant.argumento[3])

SELECT	Max(fade_secuen)
	INTO	:li_secuencia
	FROM	dbo.facturproddeta
	WHERE	clie_codigo =	:uo_SelCliente.Codigo
	AND	zona_codigo	=	:li_zona
	AND	prod_codigo	=	:ll_prod
	AND	plde_codigo	=	:uo_SelPlanta.Codigo
	AND   faen_fechaf =  :ld_fecha;

IF Isnull(li_secuencia) THEN li_secuencia = 0

ll_filas	=	dw_1.RowCount()

FOR ll_fila = 1 TO ll_filas
	IF Isnull(dw_1.Object.fade_secuen[ll_fila]) or dw_1.Object.fade_secuen[ll_fila] = 0 THEN
		li_secuencia ++
		dw_1.Object.fade_secuen[ll_fila] = li_secuencia
	END IF
NEXT
end event

event resize;call super::resize;st_7.Width = st_encabe.Width
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_facturprodgenerado
integer x = 73
integer y = 596
integer width = 3319
integer height = 1148
integer taborder = 130
string dataobject = "dw_mues_facturprodgenerado"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_facturprodgenerado
integer x = 73
integer y = 28
integer width = 3301
integer height = 420
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_facturprodgenerado
integer x = 3579
integer y = 68
integer taborder = 100
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)
uo_SelPlanta.Bloquear(True)
em_produc.Enabled	= 	False
em_fecha.Enabled		= 	False
cb_2.Enabled			= 	False
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_facturprodgenerado
integer x = 3579
integer y = 484
integer taborder = 140
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)
uo_SelPlanta.Bloquear(False)
em_produc.Enabled		= 	True
em_fecha.Enabled		= 	True
cb_2.Enabled			= 	True

pb_imprimir.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled	= False
pb_insertar.Enabled	= False
em_produc.Text		= ''
sle_nompro.Text		= ''
sle_nomzona.Text		= ''
em_cambio.Text		= ''
em_poriva.Text			= ''

uo_SelCliente.Inicia(gi_CodExport)
uo_SelPlanta.Inicia(gi_CodPlanta)

em_Fecha.Text				=	String(Today())
istr_mant.argumento[3]	= 	'01/'+em_Fecha.Text
istr_mant.argumento[5]	=	String(gi_CodEspecie)
istr_mant.argumento[6]	=	String(gi_CodVariedad)

em_produc.SetFocus()
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_facturprodgenerado
integer x = 3579
integer y = 700
integer taborder = 150
end type

event pb_insertar::clicked;istr_mant.argumento[15] = '2'

call super::clicked



end event

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_facturprodgenerado
boolean visible = false
integer x = 3579
integer y = 876
integer taborder = 160
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_facturprodgenerado
integer x = 3579
integer y = 1052
integer taborder = 170
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_facturprodgenerado
boolean visible = false
integer x = 3579
integer y = 1228
integer taborder = 180
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_facturprodgenerado
integer x = 3579
integer y = 1520
integer taborder = 190
end type

type st_3 from statictext within w_mant_mues_facturprodgenerado
integer x = 151
integer y = 316
integer width = 297
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type em_produc from editmask within w_mant_mues_facturprodgenerado
event modified pbm_enmodified
integer x = 457
integer y = 316
integer width = 219
integer height = 92
integer taborder = 10
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
string displaydata = ""
end type

event modified;Integer	li_zona, li_cliente
String	ls_productor, ls_zona
Long		ll_codigo

li_cliente	=	Integer(istr_mant.argumento[1])
ll_codigo   =  Long(This.Text)

SELECT	pro.zona_codigo, pro.prod_nombre, zon.zona_nombre
	INTO 	:li_zona, :ls_productor, :ls_zona
	FROM	dbo.productores as pro, dbo.zonas as zon
	WHERE	pro.prod_codigo = :ll_codigo
	AND	zon.zona_codigo = pro.zona_codigo;

IF sqlca.SQLCode = -1 THEN
	MessageBox("Error","Error al intentar conección a Base de Datos",Information!, Ok!)
	sle_nompro.text	= ""
	sle_nomzona.text	= ""
	This.SetFocus()
	RETURN
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error","Código de Productor no ha sido ingresado.",Information!, Ok!)
	sle_nompro.text	= ""
	sle_nomzona.text	= ""
	This.SetFocus()
	RETURN
ELSE
	istr_mant.argumento[4]	= String(ll_codigo)
	sle_nompro.text			= ls_productor
	sle_nomzona.text			= ls_zona
	istr_mant.argumento[8]	= ls_productor
	istr_mant.argumento[9]	= String(li_zona)
	istr_mant.argumento[10]	= ls_zona
	
	pb_lectura.Enabled		= True
	pb_lectura.SetFocus()
END IF
end event

type cb_2 from uo_buscar within w_mant_mues_facturprodgenerado
event clicked pbm_bnclicked
integer x = 718
integer y = 316
integer width = 96
integer height = 88
integer taborder = 40
boolean bringtotop = true
end type

event clicked;
istr_busq.argum[1] = istr_mant.argumento[1]

OpenWithParm(w_busc_productores, istr_busq)

istr_busq	= Message.PowerObjectParm

IF istr_busq.argum[1] = "" THEN
	em_produc.SetFocus()
ELSE
	em_produc.Text				= istr_busq.argum[3]
	sle_nompro.Text			= istr_busq.argum[4]
	sle_nomzona.Text			= istr_busq.argum[2]

	istr_mant.argumento[4]	= istr_busq.argum[3]
	istr_mant.argumento[8]	= istr_busq.argum[4]
	istr_mant.argumento[9]	= istr_busq.argum[7]
	istr_mant.argumento[10]	= istr_busq.argum[8]
	
	
	pb_lectura.Enabled		= True
	pb_lectura.SetFocus()
END IF
end event

type sle_nompro from singlelineedit within w_mant_mues_facturprodgenerado
integer x = 841
integer y = 316
integer width = 1170
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type sle_nomzona from singlelineedit within w_mant_mues_facturprodgenerado
integer x = 2016
integer y = 316
integer width = 585
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_mant_mues_facturprodgenerado
integer x = 151
integer y = 64
integer width = 274
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

type st_1 from statictext within w_mant_mues_facturprodgenerado
integer x = 151
integer y = 188
integer width = 274
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_facturprodgenerado
integer x = 1682
integer y = 64
integer width = 489
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
string text = "Mes Proceso"
boolean focusrectangle = false
end type

type em_fecha from editmask within w_mant_mues_facturprodgenerado
integer x = 2208
integer y = 48
integer width = 315
integer height = 96
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
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

event modified;istr_mant.argumento[3]	=	'01/'+This.Text
wf_CargaProforma(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date('01/' + This.Text))
end event

type em_cambio from editmask within w_mant_mues_facturprodgenerado
integer x = 1792
integer y = 188
integer width = 293
integer height = 92
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "###.00"
string displaydata = ""
end type

event modified;Integer	li_codigo, li_zona, li_cliente
String	ls_productor, ls_zona

li_cliente	=	Integer(istr_mant.argumento[1])
li_codigo   =  Integer(This.Text)

SELECT	pro.zona_codigo, pro.prod_nombre, zon.zona_nombre
	INTO 	:li_zona, :ls_productor, :ls_zona
	FROM	dbo.productores as pro, dbo.zonas as zon
	WHERE	pro.clie_codigo = :li_cliente
	AND   pro.prod_codigo = :li_codigo
	AND	zon.zona_codigo = pro.zona_codigo;

IF sqlca.SQLCode = -1 THEN
	MessageBox("Error","Error al intentar conección a Base de Datos",Information!, Ok!)
	sle_nompro.text	= ""
	sle_nomzona.text	= ""
	This.SetFocus()
	RETURN
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error","Código de Productor no ha sido ingresado.",Information!, Ok!)
	sle_nompro.text	= ""
	sle_nomzona.text	= ""
	This.SetFocus()
	RETURN
ELSE
	istr_mant.argumento[4]	= String(li_codigo)
	sle_nompro.text			= ls_productor
	sle_nomzona.text			= ls_zona
	istr_mant.argumento[8]	= ls_productor
	istr_mant.argumento[9]	= String(li_zona)
	istr_mant.argumento[10]	= ls_zona
	
	pb_lectura.Enabled		= True
	pb_lectura.SetFocus()
END IF
end event

type em_poriva from editmask within w_mant_mues_facturprodgenerado
integer x = 2304
integer y = 188
integer width = 215
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
alignment alignment = right!
borderstyle borderstyle = stylelowered!
string mask = "###.00"
string displaydata = ""
end type

event modified;Integer	li_codigo, li_zona, li_cliente
String	ls_productor, ls_zona

li_cliente	=	Integer(istr_mant.argumento[1])
li_codigo   =  Integer(This.Text)

SELECT	pro.zona_codigo, pro.prod_nombre, zon.zona_nombre
	INTO 	:li_zona, :ls_productor, :ls_zona
	FROM	dbo.productores as pro, dbo.zonas as zon
	WHERE	pro.clie_codigo = :li_cliente
	AND   pro.prod_codigo = :li_codigo
	AND	zon.zona_codigo = pro.zona_codigo;

IF sqlca.SQLCode = -1 THEN
	MessageBox("Error","Error al intentar conección a Base de Datos",Information!, Ok!)
	sle_nompro.text	= ""
	sle_nomzona.text	= ""
	This.SetFocus()
	RETURN
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Error","Código de Productor no ha sido ingresado.",Information!, Ok!)
	sle_nompro.text	= ""
	sle_nomzona.text	= ""
	This.SetFocus()
	RETURN
ELSE
	istr_mant.argumento[4]	= String(li_codigo)
	sle_nompro.text			= ls_productor
	sle_nomzona.text			= ls_zona
	istr_mant.argumento[8]	= ls_productor
	istr_mant.argumento[9]	= String(li_zona)
	istr_mant.argumento[10]	= ls_zona
	
	pb_lectura.Enabled		= True
	pb_lectura.SetFocus()
END IF
end event

type st_4 from statictext within w_mant_mues_facturprodgenerado
integer x = 2121
integer y = 192
integer width = 160
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
boolean enabled = false
string text = "% Iva"
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_mues_facturprodgenerado
integer x = 1463
integer y = 192
integer width = 297
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
boolean enabled = false
string text = "V.Cambio"
boolean focusrectangle = false
end type

type st_7 from statictext within w_mant_mues_facturprodgenerado
integer x = 78
integer y = 448
integer width = 3301
integer height = 136
boolean bringtotop = true
integer textsize = -10
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

type cbx_consovariedades from checkbox within w_mant_mues_facturprodgenerado
integer x = 151
integer y = 472
integer width = 759
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

type cbx_consocalibres from checkbox within w_mant_mues_facturprodgenerado
integer x = 1330
integer y = 472
integer width = 690
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

type cbx_consoembalajes from checkbox within w_mant_mues_facturprodgenerado
integer x = 2578
integer y = 472
integer width = 722
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

type em_mdesde from editmask within w_mant_mues_facturprodgenerado
integer x = 2560
integer y = 52
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
ld_Fecha			=	Date(em_Mdesde.Text)

IF Year(ld_Fecha) <> Year(ld_MesFactur) OR Month(ld_Fecha) <> Month(ld_MesFactur) THEN
	em_Mdesde.Text	=	String(ld_nulo)
	em_Mdesde.SetFocus()
END IF
end event

type em_mhasta from editmask within w_mant_mues_facturprodgenerado
integer x = 2949
integer y = 52
integer width = 352
integer height = 92
integer taborder = 110
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

type st_16 from statictext within w_mant_mues_facturprodgenerado
integer x = 2533
integer y = 208
integer width = 151
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

type em_numero from editmask within w_mant_mues_facturprodgenerado
integer x = 2679
integer y = 196
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

type cb_proforma from commandbutton within w_mant_mues_facturprodgenerado
integer x = 2939
integer y = 200
integer width = 123
integer height = 88
integer taborder = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;wf_BuscaProforma(uo_SelCliente.Codigo, uo_SelPlanta.Codigo, Date('01/' + em_fecha.Text))
end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_facturprodgenerado
event destroy ( )
integer x = 457
integer y = 48
integer height = 96
integer taborder = 90
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_facturprodgenerado
event destroy ( )
integer x = 457
integer y = 172
integer height = 96
integer taborder = 120
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

