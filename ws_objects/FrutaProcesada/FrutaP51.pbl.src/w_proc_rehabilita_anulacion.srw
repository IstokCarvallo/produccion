$PBExportHeader$w_proc_rehabilita_anulacion.srw
$PBExportComments$Genera Archivo Plano SAG de Eliminación de Pallets.
forward
global type w_proc_rehabilita_anulacion from w_mant_tabla
end type
type st_3 from statictext within w_proc_rehabilita_anulacion
end type
type dw_cliente from datawindow within w_proc_rehabilita_anulacion
end type
type st_4 from statictext within w_proc_rehabilita_anulacion
end type
type dw_planta from datawindow within w_proc_rehabilita_anulacion
end type
type st_numero from statictext within w_proc_rehabilita_anulacion
end type
type em_nrosag from editmask within w_proc_rehabilita_anulacion
end type
type dw_2 from datawindow within w_proc_rehabilita_anulacion
end type
type dw_3 from datawindow within w_proc_rehabilita_anulacion
end type
end forward

global type w_proc_rehabilita_anulacion from w_mant_tabla
integer width = 3319
integer height = 2484
string title = "Habilitación de Pallets con Anulación SAG"
st_3 st_3
dw_cliente dw_cliente
st_4 st_4
dw_planta dw_planta
st_numero st_numero
em_nrosag em_nrosag
dw_2 dw_2
dw_3 dw_3
end type
global w_proc_rehabilita_anulacion w_proc_rehabilita_anulacion

type variables
Integer			ii_PlantaSag

w_proc_deta_rehabilita_anulacion	iw_mantencion

DataWindowChild	idwc_cliente, idwc_planta
end variables

forward prototypes
public function boolean noexistecliente (integer ai_cliente)
public function boolean noexisteplanta (integer ai_planta)
public function integer codigoplantasag (integer planta)
end prototypes

public function boolean noexistecliente (integer ai_cliente);String	ls_Nombre

SELECT	clie_nombre
	INTO	:ls_Nombre
	FROM	dbo.clientesprod
	WHERE	clie_codigo	=	:ai_Cliente ;
			
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca,"Lectura de la Tabla Clientes Producción")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.", Exclamation!, Ok!)
					
	RETURN True
ELSE

	istr_mant.argumento[1]	=	String(ai_cliente)
	RETURN False
END IF
end function

public function boolean noexisteplanta (integer ai_planta);String	ls_Nombre
Integer	li_cliente

li_cliente	=	Integer(istr_mant.argumento[1])

SELECT	plde_nombre, plde_codsag
	INTO	:ls_Nombre, :ii_PlantaSag
	FROM	dbo.plantadesp
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

public function integer codigoplantasag (integer planta);Integer	li_Cliente, li_codigo

li_Cliente	=	Integer(istr_mant.Argumento[1])

SELECT	plde_codpla
	INTO	:li_Codigo
	FROM	dbo.plantadesp
	WHERE	plde_codigo	=	:planta;

RETURN li_Codigo

end function

on w_proc_rehabilita_anulacion.create
int iCurrent
call super::create
this.st_3=create st_3
this.dw_cliente=create dw_cliente
this.st_4=create st_4
this.dw_planta=create dw_planta
this.st_numero=create st_numero
this.em_nrosag=create em_nrosag
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.dw_planta
this.Control[iCurrent+5]=this.st_numero
this.Control[iCurrent+6]=this.em_nrosag
this.Control[iCurrent+7]=this.dw_2
this.Control[iCurrent+8]=this.dw_3
end on

on w_proc_rehabilita_anulacion.destroy
call super::destroy
destroy(this.st_3)
destroy(this.dw_cliente)
destroy(this.st_4)
destroy(this.dw_planta)
destroy(this.st_numero)
destroy(this.em_nrosag)
destroy(this.dw_2)
destroy(this.dw_3)
end on

event open;call super::open;x = 0
y = 0

This.Icon	=	Gstr_apl.Icono

dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve(-1)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1,"clie_codigo",gi_CodExport)

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
idwc_planta.Retrieve(-1)
dw_planta.InsertRow(0)
dw_planta.SetItem(1,"plde_codigo",gi_CodPlanta)

istr_mant.argumento[1]	=	String(gi_CodExport)
istr_mant.argumento[2]	=	String(gi_CodPlanta)
istr_mant.argumento[25]	=	'1'

dw_3.SetTransObject(sqlca)

pb_insertar.Enabled	=	True
end event

event ue_nuevo;istr_mant.borra			= False
istr_mant.agrega			= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)


end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_guardar;Integer		li_Cliente, li_Inspeccion, li_CantInspec, li_Planta, li_destino, li_tipoin
Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_Pallet, ll_NroSag, ll_filita, ll_maxinpe, ll_numinpe
String		ls_Archivo, ls_Registro, ls_NumeroSAG
Date			ld_Fecha, ld_fechai

dw_2.Reset()

dw_1.SetSort('paen_numero')
dw_1.Sort()

ld_Fecha			=	Today()	

li_Cliente		=	Integer(istr_mant.Argumento[1])
li_Planta		=	Integer(istr_mant.Argumento[2])
ii_PlantaSag	=	CodigoPlantaSag(Integer(istr_mant.Argumento[2]))
//ls_NumeroSAG	=	String(Long(em_nrosag.Text), '00000')
li_CantInspec	=	dw_1.RowCount()

//ls_Archivo	=	String(ii_PlantaSag, '000') + ls_NumeroSAG + ".REANU"
//ll_NroSag	=	Long(ls_NumeroSAG)
//ls_Registro	=	ls_NumeroSAG
//ls_Registro	+=	String(ii_PlantaSag, '0000') 
//ls_Registro	+=	String(li_CantInspec, '0000')
//ll_FilaDet	=	dw_2.InsertRow(0)

//dw_2.Object.registro[ll_FilaDet]	=	ls_Registro

FOR ll_Fila = 1 TO dw_1.RowCount()
	li_Inspeccion	=	dw_1.Object.paen_inspec[ll_Fila]
//	ls_Registro	=	String(li_Cliente, '000')
//	ls_Registro	+=	String(dw_1.Object.paen_numero[ll_Fila], '0000000')
	ll_Pallet	=	dw_1.Object.paen_numero[ll_Fila]
//	ll_FilaDet	=	dw_2.InsertRow(0)
//	dw_2.Object.registro[ll_FilaDet]	=	ls_Registro
	
	ll_numinpe = dw_1.Object.inpe_numero[ll_fila]
	

	
	SELECT dest_codigo,inpe_tipoin,inpe_fechai INTO :li_destino, :li_tipoin, :ld_fechai
	FROM dbo.inspecpalenc
	WHERE clie_codigo = :li_Cliente
	AND   plde_codigo = :li_Planta
	AND   inpe_numero = :ll_numinpe;
	
	UPDATE dbo.palletencab SET
	paen_inspec = 1,
	dest_codigo = :li_destino,
	inpe_numero = :ll_numinpe,
	inpe_tipoin = :li_tipoin,
	inpe_fechai = :ld_fechai
	WHERE clie_codigo = :li_Cliente
	AND   plde_codigo = :li_Planta
	AND   paen_numero = :ll_Pallet;	
		
	UPDATE dbo.inspecpaldet SET
	inpd_nroanu = Null,
	inpd_fechaa = Null,
	dest_codigo = :li_destino
	WHERE clie_codigo = :li_Cliente
	AND   plde_codigo = :li_Planta
	AND   paen_numero = :ll_Pallet
	AND   inpe_numero = :ll_numinpe;
	
NEXT

//	Fin de Archivo

//ls_Registro	=	"&&" 
//ll_FilaDet	=	dw_2.InsertRow(0)
//
//dw_2.Object.registro[ll_FilaDet]	=	ls_Registro
//
//IF dw_2.SaveAs(gs_disco+":\GeneradosSAG\" + ls_Archivo, Text!, False) = -1 THEN
//	MessageBox("Atención","No se pudo generar el archivo " + ls_Archivo)
//ELSE
//	MessageBox("Atención", "Archivo " + ls_Archivo + " Generado. Avise a Computación.")
//	pb_imprimir.Enabled	=	TRUE
//END IF
end event

event closequery;//
end event

event ue_borrar;call super::ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra		= True
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

event ue_imprimir;///* Este código no se hereda, debe ser sobreescrito en ventana descendiente */
//SetPointer(HourGlass!)
//
//Long		fila
//
//istr_info.titulo	= "INFORME ARCHIVO PLANO SAG DE ELIMINACIÓN DE PALLETS"
//istr_info.copias	= 1
//
//OpenWithParm(vinf, istr_info)
//
//vinf.dw_1.DataObject = "dw_info_eliminacion_pallets"
//
//vinf.dw_1.SetTransObject(sqlca)
//istr_mant.dw.ShareData(vinf.dw_1)
//
//fila = vinf.dw_1.RowCount()
//
//IF fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF fila = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//	vinf.dw_1.Modify("numero.text = '" + em_nrosag.text + "'")
//	
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//
//SetPointer(Arrow!)
end event

event ue_recuperadatos;w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")

Long 		ll_fila, ll_anulacion, ll_fila1, ll_Nuevo, ll_count
Integer	li_Cliente, li_Planta

li_Cliente		=	Integer(istr_mant.argumento[1])
li_Planta		=	Integer(istr_mant.argumento[2])

ll_anulacion = Long(em_nrosag.Text)

//ll_fila = dw_1.Retrieve(ll_anulacion,integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))
//dw_3.Retrieve(ll_anulacion,integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))
//
//IF ll_fila > 0 THEN
//	
////	FOR ll_fila1 = 1 TO dw_3.Rowcount() 
////	
////		ll_Nuevo = dw_1.InsertRow(0)
////		
////		dw_1.Object.clie_codigo[ll_Nuevo]	=	dw_3.Object.clie_codigo[ll_fila1]
////		dw_1.Object.plde_codigo[ll_Nuevo]	=	dw_3.Object.plde_codigo[ll_fila1]
////		dw_1.Object.paen_numero[ll_Nuevo]	=	dw_3.Object.paen_numero[ll_fila1]
////		dw_1.Object.paen_tipopa[ll_Nuevo]	=	dw_3.Object.paen_tipopa[ll_fila1]
////		dw_1.Object.vari_codigo[ll_Nuevo]	=	dw_3.Object.vari_codigo[ll_fila1]
////		dw_1.Object.vari_nombre[ll_Nuevo]	=	dw_3.Object.vari_nombre[ll_fila1]
////		dw_1.Object.emba_codigo[ll_Nuevo]	=	dw_3.Object.emba_codigo[ll_fila1]
////		dw_1.Object.cate_codigo[ll_Nuevo]	=	dw_3.Object.cate_codigo[ll_fila1]
////		dw_1.Object.stat_codigo[ll_Nuevo]	=	dw_3.Object.stat_codigo[ll_fila1]
////		dw_1.Object.paen_inspec[ll_Nuevo]	=	dw_3.Object.paen_inspec[ll_fila1]
////		dw_1.Object.paen_ccajas[ll_Nuevo]	=	dw_3.Object.paen_ccajas[ll_fila1]
////	NEXT
//END IF

SELECT Count()
INTO   :ll_count
FROM dbo.inspecpaldet
WHERE clie_codigo = :li_cliente
AND   plde_codigo = :li_planta
AND	isnull(inpd_nroanu,0) =	:ll_anulacion;

IF ll_count = 0 OR sqlca.SQLCode = -1 THEN
	MessageBox("Atención", "Número de Anulación No Registra Folios.~rIngrese otro.", + &
	Exclamation!, OK!)
	em_nrosag.Text	=	''
	em_nrosag.SetFocus()
	dw_1.Reset()
ELSE
	pb_imprimir.Enabled	= False
	pb_eliminar.Enabled	= False
	pb_grabar.Enabled		= False
	pb_insertar.Enabled	= True	
END IF


end event

type dw_1 from w_mant_tabla`dw_1 within w_proc_rehabilita_anulacion
integer y = 380
integer width = 2555
integer height = 1288
integer taborder = 0
string dataobject = "dw_gene_inspecpaldet_pallet"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_proc_rehabilita_anulacion
integer width = 2555
integer height = 304
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_proc_rehabilita_anulacion
boolean visible = false
integer x = 2802
integer taborder = 0
boolean enabled = false
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_proc_rehabilita_anulacion
integer x = 2798
integer taborder = 40
end type

event pb_nuevo::clicked;pb_insertar.Enabled	= False
pb_imprimir.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False

em_nrosag.Text	=	''
em_nrosag.SetFocus()
dw_1.Reset()

//istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_proc_rehabilita_anulacion
integer x = 2798
integer taborder = 30
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_proc_rehabilita_anulacion
boolean visible = false
integer x = 2798
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_proc_rehabilita_anulacion
integer x = 2798
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_proc_rehabilita_anulacion
boolean visible = false
integer x = 2798
end type

type pb_salir from w_mant_tabla`pb_salir within w_proc_rehabilita_anulacion
integer x = 2798
end type

type st_3 from statictext within w_proc_rehabilita_anulacion
integer x = 718
integer y = 140
integer width = 453
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

type dw_cliente from datawindow within w_proc_rehabilita_anulacion
integer x = 1106
integer y = 128
integer width = 1152
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event dberror;RETURN 1
end event

event itemchanged;IF NoExisteCliente(Integer(data)) THEN
	This.SetItem(1, "clie_codigo", gi_codexport)
	
	RETURN 1
ELSE
	istr_mant.argumento[1]	=	String(Integer(data), '000')

	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(sqlca)
	idwc_planta.Retrieve(1)
	istr_mant.argumento[2]	=	String(dw_planta.Object.plde_codigo[1])
	dw_planta.SetItem(1, "plde_codigo", Integer(istr_mant.argumento[2]))
END IF
end event

event itemerror;Return 1
end event

type st_4 from statictext within w_proc_rehabilita_anulacion
integer x = 718
integer y = 256
integer width = 453
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
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_proc_rehabilita_anulacion
integer x = 1106
integer y = 244
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event dberror;RETURN 1
end event

event itemchanged;IF NoExistePlanta(Integer(data)) THEN
	This.SetItem(1, "plde_codigo", gi_codplanta)
	
	RETURN 1
ELSE
	istr_mant.argumento[2]	=	String(data)
	
	pb_insertar.Enabled	=	True

END IF
end event

event itemerror;Return 1
end event

type st_numero from statictext within w_proc_rehabilita_anulacion
boolean visible = false
integer x = 1920
integer y = 2008
integer width = 695
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean enabled = false
string text = "Anulación a Habilitar"
boolean focusrectangle = false
end type

type em_nrosag from editmask within w_proc_rehabilita_anulacion
boolean visible = false
integer x = 3483
integer y = 1960
integer width = 151
integer height = 92
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;pb_insertar.Enabled	=	True

Parent.TriggerEvent("ue_recuperadatos")
end event

type dw_2 from datawindow within w_proc_rehabilita_anulacion
boolean visible = false
integer x = 110
integer y = 1608
integer width = 2555
integer height = 400
boolean bringtotop = true
string dataobject = "dw_gene_archivo_saam_plano"
boolean livescroll = true
end type

type dw_3 from datawindow within w_proc_rehabilita_anulacion
boolean visible = false
integer x = 352
integer y = 1700
integer width = 1920
integer height = 544
integer taborder = 90
boolean bringtotop = true
boolean titlebar = true
string title = "none"
string dataobject = "dw_gene_inspecpaldet_pallet"
boolean minbox = true
boolean maxbox = true
boolean hscrollbar = true
boolean vscrollbar = true
boolean resizable = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

