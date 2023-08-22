$PBExportHeader$w_gene_archivo_bodega.srw
$PBExportComments$Genera archivo Plano SAG por Recepciones de Pallets Inter Planta.
forward
global type w_gene_archivo_bodega from window
end type
type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_bodega
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_bodega
end type
type st_7 from statictext within w_gene_archivo_bodega
end type
type uo_selespecie from uo_seleccion_especie within w_gene_archivo_bodega
end type
type dw_packing from datawindow within w_gene_archivo_bodega
end type
type st_numero from statictext within w_gene_archivo_bodega
end type
type st_6 from statictext within w_gene_archivo_bodega
end type
type st_4 from statictext within w_gene_archivo_bodega
end type
type st_3 from statictext within w_gene_archivo_bodega
end type
type sle_mensa from singlelineedit within w_gene_archivo_bodega
end type
type em_fecha from editmask within w_gene_archivo_bodega
end type
type dw_1 from datawindow within w_gene_archivo_bodega
end type
type st_5 from statictext within w_gene_archivo_bodega
end type
type dw_2 from datawindow within w_gene_archivo_bodega
end type
type st_2 from statictext within w_gene_archivo_bodega
end type
type pb_salir from picturebutton within w_gene_archivo_bodega
end type
type pb_grabar from picturebutton within w_gene_archivo_bodega
end type
type cbx_pakalter from checkbox within w_gene_archivo_bodega
end type
type dw_packalter from datawindow within w_gene_archivo_bodega
end type
type st_alternativo from statictext within w_gene_archivo_bodega
end type
type st_1 from statictext within w_gene_archivo_bodega
end type
end forward

global type w_gene_archivo_bodega from window
integer width = 2606
integer height = 1536
boolean titlebar = true
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 16777215
string icon = "TABLA.ICO"
event ue_guardar ( )
event ue_buscar pbm_custom12
event ue_ordenar pbm_custom13
event ue_carga_detalle pbm_custom27
event ue_listo ( )
event ue_antesguardar pbm_custom75
event ue_seleccion pbm_custom17
event ue_imprimir pbm_custom03
event ue_genera_facturas pbm_custom04
uo_selplantas uo_selplantas
uo_selcliente uo_selcliente
st_7 st_7
uo_selespecie uo_selespecie
dw_packing dw_packing
st_numero st_numero
st_6 st_6
st_4 st_4
st_3 st_3
sle_mensa sle_mensa
em_fecha em_fecha
dw_1 dw_1
st_5 st_5
dw_2 dw_2
st_2 st_2
pb_salir pb_salir
pb_grabar pb_grabar
cbx_pakalter cbx_pakalter
dw_packalter dw_packalter
st_alternativo st_alternativo
st_1 st_1
end type
global w_gene_archivo_bodega w_gene_archivo_bodega

type variables
str_mant			istr_mant
str_busqueda	istr_busq
Boolean			ib_Anulacion
Integer			ii_PlantaSag, ii_CantTarjas, ii_CantInspec
Date				id_FechaRepa, id_FechaAcceso
Time				it_HoraAcceso

uo_seleccion_especie					iuo_selespecie

DataWindowChild	idwc_cliente, idwc_planta, idwc_packing, idwc_packalter
end variables

forward prototypes
public function boolean noexistecliente (integer ai_cliente)
public function boolean noexistefolio (long al_numero)
public function string codigodestino (long al_nropallet)
public function boolean noexisteplanta (integer ai_planta)
end prototypes

event ue_guardar();Long		ll_Fila, ll_Filas, ll_FilaDet, ll_cancaj, ll_pallet
String		ls_Archivo, ls_Registro,  ls_embalaje, ls_tipopa
Integer	li_etiqueta, li_Etiq, li_PackAlter
Date		ld_fe2, ld_Fecha

SetPointer(HourGlass!)

dw_2.reset()
dw_1.reset()

dw_2.SetTransObject(Sqlca)

ib_Anulacion=	True

ld_Fecha		=	Date(em_Fecha.Text)
li_PackAlter	=	Integer(istr_mant.argumento[5])

If ib_Anulacion Then
	ll_Filas		= dw_2.Retrieve(uo_SelCliente.Codigo, uo_SelPlantas.Codigo, ld_Fecha, &
										 Integer(istr_mant.argumento[4]), uo_SelEspecie.Codigo)
	
	If ll_Filas = -1 Then
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Detalle de Pallets")
	ElseIf ll_Filas = 0 Then
		MessageBox("Atención", "No hay información de Producción en datos indicados.~r~rPruebe Con Otro Packing.", Exclamation!, Ok!)
		pb_grabar.Enabled	= False
	Else
		dw_2.SetSort('paen_numero')
		dw_2.Sort()

		If	cbx_pakalter.Checked Then
			ls_Archivo	=	String(integer(istr_mant.argumento[5]), '0000') + &
								String(year(ld_Fecha),'0000') + String(month(ld_Fecha),'00') + String(day(ld_Fecha),'00') + &
								String(uo_SelPlantas.Codigo,'000') + "." + String(uo_SelPlantas.Codigo,'000')							
		Else
			ls_Archivo	=	String(integer(istr_mant.argumento[4]), '0000') + &
								String(year(ld_Fecha),'0000') + String(month(ld_Fecha),'00') + String(day(ld_Fecha),'00') + &
								String(uo_SelPlantas.Codigo,'000') + "." + String(uo_SelPlantas.Codigo,'000')			
		End If
		
		FOR ll_Fila = 1 TO ll_Filas
			ll_pallet	=	dw_2.Object.paen_numero[ll_Fila]
			ls_Registro	=	String(dw_2.Object.paen_numero[ll_Fila], '00000000')
			ls_Registro	+=	"0000"
			ls_Registro	+=	String(dw_2.Object.prod_codigo[ll_Fila], '000000')
			ls_Registro	+=	String(dw_2.Object.espe_codigo[ll_Fila], '00')
			ls_Registro	+=	String(dw_2.Object.vari_codigo[ll_Fila], '0000')			
			ls_Registro	+=	Left(dw_2.Object.emba_codigo[ll_Fila]+Fill(" ", 10),10)

			If dw_2.Object.paen_tipopa[ll_Fila] = 1 Then
				ls_embalaje	=	dw_2.Object.emba_codigo[ll_Fila]
				ls_tipopa	=	dw_2.Object.tpem_codigo[ll_Fila]
				
				SELECT TPEM_CANCAJ INTO :ll_cancaj
				FROM dbo.TIPOPALLEMBA
				WHERE CLIE_CODIGO = 	:uo_SelCliente.Codigo
				AND	EMBA_CODIGO	=	:ls_embalaje
				AND	TPEM_CODIGO	=	:ls_tipopa;
			Else
				ll_cancaj	=	0
			End If
			
			li_etiq	=	dw_2.Object.etiq_codigo[ll_Fila]
			
	      /**/
  			SELECT min(reet_fereet)
  			INTO :ld_fe2
				FROM dbo.reetidet
				WHERE clie_codigo=:uo_SelCliente.Codigo
				AND 	plde_codigo=:uo_SelPlantas.Codigo
				AND   paen_numero=:ll_pallet;

			If sqlca.SQLCode = 100 Then
				li_etiqueta = li_etiq;
			Else
				SELECT etiq_numant
				INTO :li_etiqueta
					FROM dbo.reetidet
					WHERE clie_codigo=:uo_SelCliente.Codigo
					AND plde_codigo=:uo_SelPlantas.Codigo
					AND paen_numero=:ll_pallet
					AND reet_fereet=:ld_fe2;
				 
				If sqlca.SQLCode = 100 Then
					li_etiqueta = li_etiq;
				End If
			End If
  			/**/
			
			ls_Registro	+= String(ll_cancaj,"000")
			ls_Registro	+=	String(dw_2.Object.tcajas[ll_Fila], '000')
			
			If	cbx_pakalter.Checked Then
				ls_Registro +=	String(li_PackAlter, '0000')				
			Else
				ls_Registro	+=	String(dw_2.Object.rfpe_ptaori[ll_Fila], '0000')
			End If

			ls_Registro	+=	String(ld_Fecha, 'YYYYMMDD')
			ls_Registro	+=	String(li_etiqueta, '000')
			ls_Registro	+=	String(dw_2.Object.paen_tipopa[ll_Fila], '0')

			ll_FilaDet	=	dw_1.InsertRow(0)
			
			dw_1.Object.registro[ll_FilaDet]	=	ls_Registro
		Next
	End If
End If

If dw_1.SaveAs(gs_disco+":\GeneradosBodega\" + ls_Archivo, Text!, False) = -1 Then
	MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
Else
	sle_mensa.text	= "Archivo " + ls_Archivo + " Generado. Avise a Computación."
End If

SetPointer(Arrow!)
end event

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

public function boolean noexistecliente (integer ai_cliente);String	ls_Nombre

SELECT	clie_nombre
	INTO	:ls_Nombre
	FROM	dbo.CLIENTESPROD
	WHERE	clie_codigo	=	:ai_Cliente ;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Clientes Producción")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
					
	RETURN True
ELSE
	RETURN False
END IF
end function

public function boolean noexistefolio (long al_numero);Integer	li_Cliente, li_Planta, li_TipoRece
//Long		ll_Numero
//
//li_Cliente	=	dw_cliente.Object.clie_codigo[1]
//li_Planta	=	dw_planta.Object.plde_codigo[1]
//ll_Numero	=	Long(em_numero.Text)
//
//
//li_Cliente	=	dw_cliente.Object.clie_codigo[1]
//li_Planta	=	dw_planta.Object.plde_codigo[1]
//
//SELECT	rfpe_fecrec, rfpe_tipoen
//	INTO	:id_FechaRepa, :li_TipoRece
//	FROM	"dbo"."RECFRUPROCEE"
//	WHERE	plde_codigo	=	:li_Planta
//	AND	rfpe_numero	=	:al_Numero ;
//			
//IF sqlca.SQLCode = -1 THEN
//	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Recpeción Fruta Procesada")
//	
//	RETURN True
//ELSEIF sqlca.SQLCode = 100 THEN
//	MessageBox("Atención", "Número de Recepción no ha sido Ingresado.~r~r" + &
//					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)
//					
//	RETURN True
//ELSEIF li_TipoRece <> 2 THEN
//	MessageBox("Atención", "Número de Recepción no Corresponde a Interplanta.~r~r" + &
//					"Ingrese o seleccione otro Número.", Exclamation!, Ok!)
//					
//	RETURN True
//ELSE
//	em_fecha.Text	=	String(id_FechaRepa)
//	
RETURN False
//END IF
end function

public function string codigodestino (long al_nropallet);String	ls_Codigo
//Integer	li_Cliente
//
//li_Cliente	=	Integer(istr_mant.Argumento[1])
//
//SELECT	dest_codigo
//	INTO	:ls_Codigo
//	FROM	"dbo"."palletencab"
//	WHERE	clie_codigo	=	:li_Cliente
//	AND	paen_numero	=	:al_NroPallet ;
//
//ls_Codigo	=	String(ls_Codigo, '000')
//
RETURN ls_Codigo
end function

public function boolean noexisteplanta (integer ai_planta);String	ls_Nombre
Integer	li_cliente

li_cliente	=	Integer(istr_mant.argumento[1])

SELECT	plde_nombre, plde_codsag
	INTO	:ls_Nombre, :ii_PlantaSag
	FROM	dbo.PLANTADESP
	WHERE	plde_codigo	=	:ai_Planta;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Plantas y Frigoríficos")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
	
	RETURN True
ELSE
	RETURN False
END IF
end function

on w_gene_archivo_bodega.create
this.uo_selplantas=create uo_selplantas
this.uo_selcliente=create uo_selcliente
this.st_7=create st_7
this.uo_selespecie=create uo_selespecie
this.dw_packing=create dw_packing
this.st_numero=create st_numero
this.st_6=create st_6
this.st_4=create st_4
this.st_3=create st_3
this.sle_mensa=create sle_mensa
this.em_fecha=create em_fecha
this.dw_1=create dw_1
this.st_5=create st_5
this.dw_2=create dw_2
this.st_2=create st_2
this.pb_salir=create pb_salir
this.pb_grabar=create pb_grabar
this.cbx_pakalter=create cbx_pakalter
this.dw_packalter=create dw_packalter
this.st_alternativo=create st_alternativo
this.st_1=create st_1
this.Control[]={this.uo_selplantas,&
this.uo_selcliente,&
this.st_7,&
this.uo_selespecie,&
this.dw_packing,&
this.st_numero,&
this.st_6,&
this.st_4,&
this.st_3,&
this.sle_mensa,&
this.em_fecha,&
this.dw_1,&
this.st_5,&
this.dw_2,&
this.st_2,&
this.pb_salir,&
this.pb_grabar,&
this.cbx_pakalter,&
this.dw_packalter,&
this.st_alternativo,&
this.st_1}
end on

on w_gene_archivo_bodega.destroy
destroy(this.uo_selplantas)
destroy(this.uo_selcliente)
destroy(this.st_7)
destroy(this.uo_selespecie)
destroy(this.dw_packing)
destroy(this.st_numero)
destroy(this.st_6)
destroy(this.st_4)
destroy(this.st_3)
destroy(this.sle_mensa)
destroy(this.em_fecha)
destroy(this.dw_1)
destroy(this.st_5)
destroy(this.dw_2)
destroy(this.st_2)
destroy(this.pb_salir)
destroy(this.pb_grabar)
destroy(this.cbx_pakalter)
destroy(this.dw_packalter)
destroy(this.st_alternativo)
destroy(this.st_1)
end on

event open;Boolean	lb_Cerrar
x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

dw_packing.GetChild("plde_codigo", idwc_packing)
idwc_packing.SetTransObject(sqlca)
idwc_packing.Retrieve(gi_codexport,gi_codplanta,Date(em_fecha.Text))
dw_packing.InsertRow(0)
dw_packing.SetItem(1,"plde_codigo",0)

dw_packalter.GetChild("plde_codigo", idwc_packalter)
idwc_packalter.SetTransObject(sqlca)
idwc_packalter.Retrieve(2)
dw_packalter.InsertRow(0)
dw_packalter.SetItem(1,"plde_codigo",0)
dw_packalter.Object.plde_codigo.BackGround.Color	=	RGB(255,255,255)
dw_packalter.Object.plde_codigo.BackGround.Color	=	553648127


If lb_Cerrar Then
	Close(This)
Else
	uo_SelEspecie.Seleccion(True,False)
	uo_SelCliente.Seleccion(False,False)
	uo_SelPlantas.Seleccion(False,False)
	
	uo_SelPlantas.Filtra(1)
	
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)
	
	em_fecha.Text				=	String(Today())
	
	istr_mant.argumento[3]	=	em_fecha.Text
	istr_mant.argumento[4]	=	""
	istr_mant.argumento[5]	=	""
	
	GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)
End If
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)

end event

type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_bodega
event destroy ( )
integer x = 750
integer y = 424
integer height = 84
integer taborder = 40
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;	idwc_packing.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,Date(em_fecha.text))
	istr_mant.argumento[4]	=	String(dw_packing.Object.plde_codigo[1],'0000')
	
	idwc_packalter.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,Date(em_fecha.text))
	istr_mant.argumento[5]	=	String(dw_packalter.Object.plde_codigo[1],'0000')
end event

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_bodega
event destroy ( )
integer x = 750
integer y = 300
integer height = 84
integer taborder = 30
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

event ue_cambio;call super::ue_cambio;idwc_packing.Retrieve(This.Codigo,uo_SelPlantas.Codigo,Date(em_fecha.text))
istr_mant.argumento[4]	=	String(dw_packing.Object.plde_codigo[1],'0000')

idwc_packalter.Retrieve(This.Codigo,uo_SelPlantas.Codigo,Date(em_fecha.text))
istr_mant.argumento[5]	=	String(dw_packalter.Object.plde_codigo[1],'0000')
end event

type st_7 from statictext within w_gene_archivo_bodega
integer x = 123
integer y = 864
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

type uo_selespecie from uo_seleccion_especie within w_gene_archivo_bodega
event destroy ( )
integer x = 750
integer y = 792
integer height = 180
integer taborder = 90
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

event ue_cambio;call super::ue_cambio;pb_grabar.Enabled			=	True
end event

type dw_packing from datawindow within w_gene_archivo_bodega
integer x = 750
integer y = 688
integer width = 969
integer height = 92
integer taborder = 40
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_prod_dia"
boolean border = false
boolean livescroll = true
end type

event dberror;RETURN 1
end event

event itemchanged;IF NoExistePlanta(Integer(data)) THEN
	This.SetItem(1, "plde_codigo", 0)
	cbx_pakalter.Enabled	=	False	
	cbx_pakalter.Checked	=  False
	dw_packalter.Enabled	=  False	
	pb_grabar.Enabled		=	False
	RETURN 1
ELSE
	istr_mant.argumento[4]	=	String(Integer(data), '0000')
	cbx_pakalter.Enabled		=	True
	cbx_pakalter.Checked		= False
	dw_packalter.Enabled		= False
	pb_grabar.Enabled			=	True
END IF






end event

type st_numero from statictext within w_gene_archivo_bodega
integer x = 123
integer y = 556
integer width = 485
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Fecha Embalaje"
boolean focusrectangle = false
end type

type st_6 from statictext within w_gene_archivo_bodega
integer x = 78
integer y = 1228
integer width = 1970
integer height = 176
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_gene_archivo_bodega
integer x = 123
integer y = 432
integer width = 517
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Planta Receptora"
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_archivo_bodega
integer x = 123
integer y = 308
integer width = 311
integer height = 64
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

type sle_mensa from singlelineedit within w_gene_archivo_bodega
integer x = 178
integer y = 1268
integer width = 1787
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
boolean italic = true
long textcolor = 65535
long backcolor = 553648127
boolean autohscroll = false
boolean displayonly = true
borderstyle borderstyle = stylelowered!
end type

type em_fecha from editmask within w_gene_archivo_bodega
integer x = 750
integer y = 552
integer width = 512
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
string displaydata = "@"
boolean dropdowncalendar = true
end type

event modified;idwc_packing.Retrieve(uo_SelCliente.Codigo,uo_SelPlantas.Codigo,Date(This.text))
dw_packing.InsertRow(0)
dw_packing.SetItem(1,"plde_codigo",0)

istr_mant.argumento[4]	=	String(dw_packing.Object.plde_codigo[1],'0000')

cbx_pakalter.Enabled	=	False	
cbx_pakalter.Checked	=	False
dw_packalter.Enabled	= 	False	

dw_packalter.InsertRow(0)
dw_packalter.SetItem(1,"plde_codigo",0)
dw_packalter.Object.plde_codigo.Color	=	RGB(255,255,255)
dw_packalter.Object.plde_codigo.BackGround.Color	=	553648127

end event

type dw_1 from datawindow within w_gene_archivo_bodega
boolean visible = false
integer x = 2107
integer y = 76
integer width = 343
integer height = 244
string dataobject = "dw_gene_archivo_bodega"
boolean livescroll = true
end type

type st_5 from statictext within w_gene_archivo_bodega
integer x = 78
integer y = 68
integer width = 1970
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Genera Archivo Plano para Bodega"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_2 from datawindow within w_gene_archivo_bodega
boolean visible = false
integer x = 2117
integer y = 376
integer width = 343
integer height = 244
string dataobject = "dw_mues_recfruproced_bodega_genera"
boolean border = false
end type

event clicked;This.Print()
end event

type st_2 from statictext within w_gene_archivo_bodega
integer x = 123
integer y = 700
integer width = 535
integer height = 84
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Packing Origen"
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_archivo_bodega
integer x = 2162
integer y = 1036
integer width = 343
integer height = 244
integer taborder = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Cancelar.png"
alignment htextalign = left!
end type

event clicked;Close(Parent)
end event

type pb_grabar from picturebutton within w_gene_archivo_bodega
integer x = 2158
integer y = 744
integer width = 343
integer height = 244
integer taborder = 70
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = left!
end type

event clicked;Parent.TriggerEvent("ue_guardar")
end event

type cbx_pakalter from checkbox within w_gene_archivo_bodega
integer x = 823
integer y = 1008
integer width = 782
integer height = 64
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cambia Código Packing"
end type

event clicked;IF This.Checked THEN
	pb_grabar.Enabled			=	False
	
	dw_packalter.Enabled		=	True	
	st_alternativo.Enabled	=	True

	dw_packalter.GetChild("plde_codigo", idwc_packalter)
	idwc_packalter.SetTransObject(sqlca)
	//idwc_packalter.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[2]),Date(em_fecha.text))	
	idwc_packalter.Retrieve(2)	
	dw_packalter.InsertRow(0)
	dw_packalter.SetItem(1,"plde_codigo",0)
	
	dw_packalter.Object.plde_codigo.BackGround.Color	=	RGB(255, 255, 255)
	dw_packalter.SetFocus()
	istr_mant.argumento[5]	=	String(dw_packalter.Object.plde_codigo[1],'0000')
ELSE	
	pb_grabar.Enabled			=	True
	
	istr_mant.argumento[5]	=	''
	st_alternativo.Enabled	=	False
	dw_packalter.Enabled		=	False
	dw_packalter.Object.plde_codigo.Color					=	RGB(255,255,255)
	dw_packalter.Object.plde_codigo.BackGround.Color	=	553648127
	dw_packalter.Reset()
	dw_packalter.InsertRow(0)	
	dw_packalter.SetItem(1,"plde_codigo",0)	
END IF
end event

type dw_packalter from datawindow within w_gene_archivo_bodega
integer x = 818
integer y = 1080
integer width = 1010
integer height = 92
integer taborder = 60
boolean enabled = false
string title = "none"
string dataobject = "dddw_plantadesp_packing"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF NoExistePlanta(Integer(data)) THEN
	This.SetItem(1, "plde_codigo", 0)
	istr_mant.argumento[5]	=	''
	//pb_grabar.Enabled	=	False
	RETURN 1
ELSE
	istr_mant.argumento[5]	=	String(Integer(data), '0000')
	pb_grabar.Enabled			=	True
END IF
end event

type st_alternativo from statictext within w_gene_archivo_bodega
integer x = 270
integer y = 1088
integer width = 562
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Packing Rebaje"
alignment alignment = center!
boolean focusrectangle = false
boolean disabledlook = true
end type

type st_1 from statictext within w_gene_archivo_bodega
integer x = 78
integer y = 220
integer width = 1970
integer height = 1008
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

