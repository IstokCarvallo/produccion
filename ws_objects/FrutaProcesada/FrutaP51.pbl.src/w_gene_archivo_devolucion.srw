$PBExportHeader$w_gene_archivo_devolucion.srw
$PBExportComments$Genera Archivo Plano SAG de Eliminación de Pallets.
forward
global type w_gene_archivo_devolucion from w_mant_tabla
end type
type st_3 from statictext within w_gene_archivo_devolucion
end type
type st_4 from statictext within w_gene_archivo_devolucion
end type
type st_numero from statictext within w_gene_archivo_devolucion
end type
type em_nrosag from editmask within w_gene_archivo_devolucion
end type
type dw_2 from datawindow within w_gene_archivo_devolucion
end type
type dw_3 from datawindow within w_gene_archivo_devolucion
end type
type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_devolucion
end type
type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_devolucion
end type
end forward

global type w_gene_archivo_devolucion from w_mant_tabla
integer width = 3374
integer height = 1972
string title = "Devolución de Pallets SAG"
st_3 st_3
st_4 st_4
st_numero st_numero
em_nrosag em_nrosag
dw_2 dw_2
dw_3 dw_3
uo_selcliente uo_selcliente
uo_selplantas uo_selplantas
end type
global w_gene_archivo_devolucion w_gene_archivo_devolucion

type variables
w_gene_deta_archivo_devolucion	iw_mantencion
end variables

on w_gene_archivo_devolucion.create
int iCurrent
call super::create
this.st_3=create st_3
this.st_4=create st_4
this.st_numero=create st_numero
this.em_nrosag=create em_nrosag
this.dw_2=create dw_2
this.dw_3=create dw_3
this.uo_selcliente=create uo_selcliente
this.uo_selplantas=create uo_selplantas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_3
this.Control[iCurrent+2]=this.st_4
this.Control[iCurrent+3]=this.st_numero
this.Control[iCurrent+4]=this.em_nrosag
this.Control[iCurrent+5]=this.dw_2
this.Control[iCurrent+6]=this.dw_3
this.Control[iCurrent+7]=this.uo_selcliente
this.Control[iCurrent+8]=this.uo_selplantas
end on

on w_gene_archivo_devolucion.destroy
call super::destroy
destroy(this.st_3)
destroy(this.st_4)
destroy(this.st_numero)
destroy(this.em_nrosag)
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.uo_selcliente)
destroy(this.uo_selplantas)
end on

event open;call super::open;Boolean	lb_Cerrar 

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlantas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	x = 0
	y = 0
	
	This.Icon	=	Gstr_apl.Icono
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlantas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlantas.Inicia(gi_CodPlanta)

	istr_mant.argumento[25]	=	'1'
	
	dw_3.SetTransObject(sqlca)
End If
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

event ue_guardar;Integer		li_Inspeccion, li_CantInspec
Long			ll_Fila, ll_Filas, ll_FilaDet, ll_Numero, ll_Pallet, ll_NroSag, ll_filita, &
				ll_maxinpe, ll_numinpe
String			ls_Archivo, ls_Registro, ls_NumeroSAG, ls_certificado
Date			ld_Fecha

dw_2.Reset()

dw_1.SetSort('paen_numero')
dw_1.Sort()

ld_Fecha			=	Today()	

ls_NumeroSAG	=	String(Long(em_nrosag.Text), '00000')
li_CantInspec	=	dw_1.RowCount()

IF Integer(uo_SelPlantas.Region) <>  4 THEN
	ls_Archivo	=	String(uo_SelPlantas.PlantaSAG, '000') + ls_NumeroSAG + ".DET"
ELSE
	ls_Archivo	=	String(uo_SelPlantas.PlantaSAG, '0000') + ls_NumeroSAG + ".DET"
END IF	
	
ll_NroSag	=	Long(ls_NumeroSAG)
ls_Registro	=	ls_NumeroSAG
ls_Registro	+=	String(uo_SelPlantas.PlantaSAG, '0000') 
ls_Registro	+=	String(li_CantInspec, '0000')
ll_FilaDet	=	dw_2.InsertRow(0)

dw_2.Object.registro[ll_FilaDet]	=	ls_Registro

FOR ll_Fila = 1 TO dw_1.RowCount()
	li_Inspeccion	=	dw_1.Object.paen_inspec[ll_Fila]
	ls_Registro	=	f_ClienteRotulado(uo_SelCliente.Codigo)//String(li_Cliente, '000')
	ls_Registro	+=	String(dw_1.Object.paen_numero[ll_Fila], '0000000')
	ll_Pallet	=	dw_1.Object.paen_numero[ll_Fila]
	ll_FilaDet	=	dw_2.InsertRow(0)
	dw_2.Object.registro[ll_FilaDet]	=	ls_Registro
	
	ll_numinpe 		= dw_1.Object.inpe_numero[ll_fila]
	ls_certificado	= dw_1.Object.inpd_nrocer[ll_fila]
	
	UPDATE dbo.inspecpaldet SET
	inpd_nrodev = :ll_NroSag,
	inpd_fecdev = :ld_fecha,
	inpd_nrocer = :ls_certificado
	WHERE clie_codigo = :uo_SelCliente.Codigo
	AND   plde_codigo = :uo_SelPlantas.Codigo
	AND   paen_numero = :ll_Pallet
	AND 	inpe_numero = :ll_numinpe;
	
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla inspecpaldet")
	END IF
NEXT

//	Fin de Archivo

ls_Registro	=	"&&" 
ll_FilaDet	=	dw_2.InsertRow(0)

dw_2.Object.registro[ll_FilaDet]	=	ls_Registro
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

event ue_imprimir;/* Este código no se hereda, debe ser sobreescrito en ventana descendiente */
SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "INFORME ARCHIVO PLANO SAG DE DEVOLUCIÓN DE PALLETS"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_devolucion_pallets"

vinf.dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(vinf.dw_1)

fila = vinf.dw_1.RowCount()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	
	vinf.dw_1.Modify("numero.text = '" + em_nrosag.text + "'")
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos;w_main.SetMicroHelp("Recuperando Datos...")
SetPointer(HourGlass!)
PostEvent("ue_listo")
Long 	ll_fila, ll_anulacion, ll_fila1, ll_Nuevo

ll_anulacion = Long(em_nrosag.Text)

ll_fila = dw_1.Retrieve(ll_anulacion,integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))
dw_3.Retrieve(ll_anulacion,integer(istr_mant.argumento[1]),integer(istr_mant.argumento[2]))

IF ll_fila > 0 THEN
	
//	FOR ll_fila1 = 1 TO dw_3.Rowcount() 
//	
//		ll_Nuevo = dw_1.InsertRow(0)
//		
//		dw_1.Object.clie_codigo[ll_Nuevo]	=	dw_3.Object.clie_codigo[ll_fila1]
//		dw_1.Object.plde_codigo[ll_Nuevo]	=	dw_3.Object.plde_codigo[ll_fila1]
//		dw_1.Object.paen_numero[ll_Nuevo]	=	dw_3.Object.paen_numero[ll_fila1]
//		dw_1.Object.paen_tipopa[ll_Nuevo]	=	dw_3.Object.paen_tipopa[ll_fila1]
//		dw_1.Object.vari_codigo[ll_Nuevo]	=	dw_3.Object.vari_codigo[ll_fila1]
//		dw_1.Object.vari_nombre[ll_Nuevo]	=	dw_3.Object.vari_nombre[ll_fila1]
//		dw_1.Object.emba_codigo[ll_Nuevo]	=	dw_3.Object.emba_codigo[ll_fila1]
//		dw_1.Object.cate_codigo[ll_Nuevo]	=	dw_3.Object.cate_codigo[ll_fila1]
//		dw_1.Object.stat_codigo[ll_Nuevo]	=	dw_3.Object.stat_codigo[ll_fila1]
//		dw_1.Object.paen_inspec[ll_Nuevo]	=	dw_3.Object.paen_inspec[ll_fila1]
//		dw_1.Object.paen_ccajas[ll_Nuevo]	=	dw_3.Object.paen_ccajas[ll_fila1]
//	NEXT
END IF

pb_imprimir.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_insertar.Enabled	= True
end event

type dw_1 from w_mant_tabla`dw_1 within w_gene_archivo_devolucion
integer y = 548
integer width = 2555
integer height = 1120
integer taborder = 0
string dataobject = "dw_gene_inspecpaldet_pallet_devolucion"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_gene_archivo_devolucion
integer width = 2555
integer height = 436
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_gene_archivo_devolucion
boolean visible = false
integer x = 2802
integer taborder = 0
boolean enabled = false
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_gene_archivo_devolucion
integer x = 2798
integer taborder = 50
end type

event pb_nuevo::clicked;pb_insertar.Enabled	= False
pb_imprimir.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False

em_nrosag.SetFocus()
dw_1.Reset()

//istr_mant.Solo_Consulta	=	istr_mant.UsuarioSoloConsulta
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_gene_archivo_devolucion
integer x = 2798
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_gene_archivo_devolucion
boolean visible = false
integer x = 2798
integer taborder = 60
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_gene_archivo_devolucion
integer x = 2798
integer taborder = 70
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_gene_archivo_devolucion
boolean visible = false
integer x = 2798
integer taborder = 80
end type

type pb_salir from w_mant_tabla`pb_salir within w_gene_archivo_devolucion
integer x = 2798
integer taborder = 90
end type

type st_3 from statictext within w_gene_archivo_devolucion
integer x = 393
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

type st_4 from statictext within w_gene_archivo_devolucion
integer x = 393
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

type st_numero from statictext within w_gene_archivo_devolucion
integer x = 393
integer y = 372
integer width = 695
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
string text = "Nro. Devolución  S.A.G."
boolean focusrectangle = false
end type

type em_nrosag from editmask within w_gene_archivo_devolucion
integer x = 1106
integer y = 360
integer width = 462
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16777215
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;pb_insertar.Enabled	=	True

Parent.TriggerEvent("ue_recuperadatos")
end event

type dw_2 from datawindow within w_gene_archivo_devolucion
boolean visible = false
integer x = 110
integer y = 1608
integer width = 2555
integer height = 400
boolean bringtotop = true
string dataobject = "dw_gene_archivo_saam_plano"
boolean livescroll = true
end type

type dw_3 from datawindow within w_gene_archivo_devolucion
boolean visible = false
integer x = 603
integer y = 1564
integer width = 1920
integer height = 544
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_gene_inspecpaldet_pallet_devolucion"
borderstyle borderstyle = stylelowered!
end type

type uo_selcliente from uo_seleccion_clientesprod within w_gene_archivo_devolucion
event destroy ( )
integer x = 1106
integer y = 124
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplantas from uo_seleccion_plantas within w_gene_archivo_devolucion
event destroy ( )
integer x = 1106
integer y = 240
integer height = 96
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplantas.destroy
call uo_seleccion_plantas::destroy
end on

