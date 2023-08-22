$PBExportHeader$w_aprobacion_cajas.srw
forward
global type w_aprobacion_cajas from w_mant_directo
end type
type st_1 from statictext within w_aprobacion_cajas
end type
type st_2 from statictext within w_aprobacion_cajas
end type
type iuo_selcliente from uo_seleccion_clientesprod within w_aprobacion_cajas
end type
type iuo_selplanta from uo_seleccion_plantas within w_aprobacion_cajas
end type
type st_3 from statictext within w_aprobacion_cajas
end type
type st_4 from statictext within w_aprobacion_cajas
end type
type sle_proceso from singlelineedit within w_aprobacion_cajas
end type
type ddlb_tipoproc from dropdownlistbox within w_aprobacion_cajas
end type
type st_5 from statictext within w_aprobacion_cajas
end type
type sle_mensa from singlelineedit within w_aprobacion_cajas
end type
type dw_2 from datawindow within w_aprobacion_cajas
end type
type dw_3 from datawindow within w_aprobacion_cajas
end type
end forward

global type w_aprobacion_cajas from w_mant_directo
integer width = 3639
string title = "APROBACION DE ADHESIVOS PARA PROCESO"
event ue_levantapdf ( integer ai_row )
event ue_validapassword ( )
st_1 st_1
st_2 st_2
iuo_selcliente iuo_selcliente
iuo_selplanta iuo_selplanta
st_3 st_3
st_4 st_4
sle_proceso sle_proceso
ddlb_tipoproc ddlb_tipoproc
st_5 st_5
sle_mensa sle_mensa
dw_2 dw_2
dw_3 dw_3
end type
global w_aprobacion_cajas w_aprobacion_cajas

type variables
Str_info								lstr_info

DataWindowChild	 				idwc_cliente, idwc_planta

uo_plantadesp						iuo_plantas
uo_cliente							iuo_clientes
uo_lotescorrelequipo_gr			iuo_correl
uo_aprobacioncompactospdf	iuo_CompactoPdf

String									is_Computador
Integer    							ii_tiponum, ii_cliente, ii_estado


end variables

forward prototypes
public function boolean buscaorden (long al_orden, integer ai_tipo)
protected function boolean wf_actualiza_db ()
public subroutine muestrapdf (integer ai_fila)
public subroutine habilitaencab (boolean habilita)
public function boolean existesalida ()
end prototypes

event ue_validapassword();str_mant	lstr_mant

lstr_mant.Argumento[1]	=	"Granel"
lstr_mant.Argumento[2]	=	gstr_paramplanta.Password

OpenWithParm(w_password, lstr_mant)

lstr_mant	=	Message.PowerObjectParm

IF lstr_mant.Respuesta = 0 THEN Close(This)
end event

public function boolean buscaorden (long al_orden, integer ai_tipo);Integer li_especie, li_variedad,  li_estado
String  ls_productor, ls_especie, ls_variedad
Date    ld_fecha
Long    ll_productor

SELECT prod_codigo, espe_codigo, vari_codigo, orpr_fecpro, orpr_estado
  INTO :ll_productor, :li_especie, :li_variedad, :ld_fecha, :li_estado
  FROM dbo.spro_ordenproceso
 WHERE plde_codigo	=	:iuo_SelPlanta.Codigo
   AND orpr_tipord	=	:ai_tipo
	AND orpr_numero	=	:al_orden
	AND clie_codigo	= 	:iuo_SelCliente.Codigo;
		 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Spro_Ordenproceso")
	RETURN FALSE
ELSEIF sqlca.SQLCode <> 100 THEN
   IF li_estado = 1 THEN
   	sle_mensa.text    =  'Vigente'
		Return True
	ELSEIF li_estado = 2 THEN
		sle_mensa.text    =  'Confir. Packing'
		Return True
	ELSEIF li_estado = 3 THEN
		sle_mensa.text    =  'Cerrada'
		Messagebox("Error", "Esta orden de proceso esta cerrada, imposible validar compactos", StopSign!)
		Return False
	ELSEIF li_estado = 5 THEN
		sle_mensa.Text = "Cierre Web"
		Messagebox("Error", "Esta orden de proceso esta publicada a los productores, imposible validar compactos", StopSign!)
		Return False
	ELSE
		sle_mensa.text = "Cerrada - Modificada desde Packing"
	END IF
END IF

RETURN FALSE
end function

protected function boolean wf_actualiza_db ();Long		ll_CajaInicial, ll_CajaFinal
Boolean	lb_retorno = False
String 	ls_archivo, ls_condicion

CHOOSE CASE ii_estado
	CASE 1
		ls_condicion	=	"Aprobados"
	CASE ELSE
		ls_condicion	=	"Rechazados"

END CHOOSE

ll_CajaInicial	=	dw_1.Object.capr_numero[1]
ll_CajaFinal	=	dw_1.Object.capr_numero[dw_1.RowCount()]
ls_archivo		=	sle_proceso.Text + '.'

UPDATE dbo.spro_ordenprocdeta_cajasprod
	SET capr_estado = :ii_estado
WHERE capr_numero BETWEEN :ll_CajaInicial AND :ll_CajaFinal
USING SQLCA;

IF sqlca.SQLCode <> 0 THEN
	F_ErrorBaseDatos(sqlca, This.Title)
	lb_Retorno	=	False
	RollBack;
	MessageBox("Error de Datos", "Existe un problema con los datos de los Compactos, imposible cambiar estado")

ELSE
	lb_Retorno	=	True
	Commit;
	MessageBox("Grabación Exitosa", "Los Compactos han sido " + ls_condicion)
END IF
	
Return lb_retorno
end function

public subroutine muestrapdf (integer ai_fila);String	ls_archivo

ls_archivo	=	'c:\' + sle_proceso.Text + '.' + String(dw_1.Object.capr_numero[ai_fila]) + '.pdf'

iuo_CompactoPdf.RecuperaImagen(dw_1, ai_fila, sqlca)
end subroutine

public subroutine habilitaencab (boolean habilita);IF Habilita THEN
	ddlb_tipoproc.Enabled	=	False
	sle_proceso.Enabled		=	False
	pb_lectura.Enabled		=	False
	
ELSE
	ddlb_tipoproc.Enabled	=	True
	sle_proceso.Enabled		=	True
	pb_lectura.Enabled		=	True
	pb_insertar.Enabled		=	False
	pb_eliminar.Enabled		=	False	
END IF
end subroutine

public function boolean existesalida ();Long	ll_correl

SELECT loco_comcor
  INTO :ll_correl
  FROM dbo.spro_correlcompequipo
 WHERE plde_codigo = :iuo_SelPlanta.Codigo
   AND equi_nombre = :is_Computador
 USING SQLCA;
 
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla spro_correlcompequipo")
	RETURN FALSE
ELSEIF sqlca.SQLCode <> 100 AND ll_correl > 0 AND NOT IsNull(ll_correl) THEN
	RETURN TRUE
ELSE
	MessageBox("Error de Validación", "El PC actual (" + is_Computador + &
					"), no se encuentra habilitado para la emisión de compactos", StopSign!)
	RETURN FALSE
END IF
end function

on w_aprobacion_cajas.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.iuo_selcliente=create iuo_selcliente
this.iuo_selplanta=create iuo_selplanta
this.st_3=create st_3
this.st_4=create st_4
this.sle_proceso=create sle_proceso
this.ddlb_tipoproc=create ddlb_tipoproc
this.st_5=create st_5
this.sle_mensa=create sle_mensa
this.dw_2=create dw_2
this.dw_3=create dw_3
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.iuo_selcliente
this.Control[iCurrent+4]=this.iuo_selplanta
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.sle_proceso
this.Control[iCurrent+8]=this.ddlb_tipoproc
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.sle_mensa
this.Control[iCurrent+11]=this.dw_2
this.Control[iCurrent+12]=this.dw_3
end on

on w_aprobacion_cajas.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.iuo_selcliente)
destroy(this.iuo_selplanta)
destroy(this.st_3)
destroy(this.st_4)
destroy(this.sle_proceso)
destroy(this.ddlb_tipoproc)
destroy(this.st_5)
destroy(this.sle_mensa)
destroy(this.dw_2)
destroy(this.dw_3)
end on

event open;call super::open;Boolean lb_Cerrar

IF IsNull(iuo_SelCliente.Codigo) 	THEN lb_Cerrar	=	True
IF IsNull(iuo_SelPlanta.Codigo) 	THEN lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else
	iuo_SelCliente.Seleccion(False, False)
	iuo_SelPlanta.Seleccion(False, False)
	
	iuo_SelCliente.Inicia(gi_CodExport)
	iuo_SelPlanta.Inicia(gstr_ParamPlanta.CodigoPlanta)
	
	iuo_CompactoPdf =	Create uo_aprobacioncompactospdf
	
	RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, is_Computador)
	
	ddlb_tipoproc.SelectItem(1)
	ii_tiponum	=	4
	
	dw_1.SetRowFocusIndicator(FocusRect!)
	dw_2.SetTransObject(SQLCA)
	dw_3.SetTransObject(SQLCA)
	
	IF NOT IsNull(gstr_paramplanta.Password) AND Trim(gstr_paramplanta.Password) <> '' THEN PostEvent("ue_validapassword")	
	IF NOT ExisteSalida() THEN Close(This)
End If
end event

event ue_recuperadatos;call super::ue_recuperadatos;Integer	li_filas, respuesta, li_compacto
Long		ll_caja
String	ls_fecha, ls_codigo, ls_archivo

DO
	li_filas	=	dw_1.Retrieve(iuo_SelPlanta.codigo, iuo_SelCliente.Codigo, ii_tiponum, Long(sle_proceso.Text))
	
	IF li_filas = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSE
		IF li_filas < 1 THEN
			li_filas	=	dw_3.Retrieve(iuo_SelPlanta.codigo, iuo_SelCliente.Codigo, ii_tiponum, Long(sle_proceso.Text), &
									  is_Computador, -1, gstr_us.Nombre)
			IF li_filas < 1 THEN
				MessageBox(	"Error de Datos", "No es posible crear adhesivos modelos para la orden ingresada.")
			ELSE
				li_filas	=	dw_1.Retrieve(iuo_SelPlanta.codigo, iuo_SelCliente.Codigo, ii_tiponum, Long(sle_proceso.Text))
			END IF
		END IF
		
		IF li_filas > 0 THEN
			FOR li_filas = 1 TO dw_1.RowCount()
				
				IF dw_1.Object.capr_estado[li_filas] = 0 THEN
					ll_caja		=	dw_1.Object.capr_numero[li_filas]
					li_compacto	=	dw_2.Retrieve(iuo_SelCliente.Codigo, iuo_SelPlanta.codigo, ll_caja, ll_caja, 1)
					
					IF li_compacto > 0 THEN
						ls_fecha			=	String(dw_2.Object.capr_fecemb[1])
						ls_fecha			=	Right(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Left(ls_fecha, 2)
						
						ls_codigo			=	"(01)" + dw_2.Object.emba_nroint[1] + "(10)" + ls_fecha
						ls_codigo			=	ls_codigo	+	"(21)" + String(dw_2.Object.plde_codigo[1], "0000") 
						ls_codigo			=	ls_codigo	+	String(ll_caja, '00000000')
						
					//	dw_2.Object.t_codigo.Text[1] 			= 	ls_codigo
						
						ls_archivo	=	'c:\' + sle_proceso.Text + '.' + String(ll_caja) + '.pdf'
						
						IF dw_2.SaveAs(ls_archivo, PDF!, False) = -1 THEN
							MessageBox('Atención', 'No se pudo generar Documento.')
						ELSE
							IF iuo_CompactoPdf.GrabaImagen(dw_1, li_filas, Sqlca, ls_archivo) THEN
								MessageBox("Error de Datos", "No ha sido posible actualizar el archivo dentro de la base de datos")
							END IF
							FileDelete(ls_archivo)
						END IF
						dw_2.Reset()
					ELSE
						MessageBox(	"Error de Datos", "No es posible crear adhesivo para la caja " + String(ll_caja) + ".")
					END IF
				END IF
				
			NEXT
			HabilitaEncab(True)
			
		END IF
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

type st_encabe from w_mant_directo`st_encabe within w_aprobacion_cajas
integer width = 2807
integer height = 324
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_aprobacion_cajas
integer x = 3154
integer taborder = 70
end type

event pb_nuevo::clicked;call super::clicked;iuo_SelPlanta.Bloquear(False)
iuo_SelCliente.Bloquear(False)
HabilitaEncab(False)
end event

type pb_lectura from w_mant_directo`pb_lectura within w_aprobacion_cajas
integer x = 3154
integer taborder = 60
end type

event pb_lectura::clicked;call super::clicked;iuo_SelPlanta.Bloquear(True)
iuo_SelCliente.Bloquear(True)
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_aprobacion_cajas
string tag = "Rechaza Compactos"
integer x = 3154
integer taborder = 90
end type

event pb_eliminar::clicked;ii_estado	=	2
Parent.TriggerEvent("ue_guardar")
end event

type pb_insertar from w_mant_directo`pb_insertar within w_aprobacion_cajas
string tag = "Aprueba Compactos"
integer x = 3154
integer taborder = 80
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar-bn.png"
end type

event pb_insertar::clicked;ii_estado	=	1
Parent.TriggerEvent("ue_guardar")
end event

type pb_salir from w_mant_directo`pb_salir within w_aprobacion_cajas
integer x = 3154
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_aprobacion_cajas
string tag = "Rechaza Compactos"
boolean visible = false
integer x = 3154
integer taborder = 110
end type

event pb_imprimir::clicked;ii_estado	=	2
Parent.TriggerEvent("ue_guardar")
end event

type pb_grabar from w_mant_directo`pb_grabar within w_aprobacion_cajas
string tag = "Aprueba Compactos"
boolean visible = false
integer x = 3154
integer taborder = 100
end type

event pb_grabar::clicked;ii_estado	=	1
Parent.TriggerEvent("ue_guardar")
end event

type dw_1 from w_mant_directo`dw_1 within w_aprobacion_cajas
integer y = 428
integer width = 2830
integer height = 1296
integer taborder = 50
string dataobject = "dw_mues_cajas_aprobadas"
end type

event dw_1::clicked;call super::clicked;This.SelectRow(0, False)
This.SelectRow(Row, True)

end event

event dw_1::doubleclicked;call super::doubleclicked;MuestraPDF(Row)
end event

type st_1 from statictext within w_aprobacion_cajas
integer x = 183
integer y = 112
integer width = 238
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type st_2 from statictext within w_aprobacion_cajas
integer x = 1605
integer y = 112
integer width = 219
integer height = 72
boolean bringtotop = true
integer textsize = -9
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

type iuo_selcliente from uo_seleccion_clientesprod within w_aprobacion_cajas
integer x = 645
integer y = 104
integer height = 92
integer taborder = 10
boolean bringtotop = true
end type

on iuo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type iuo_selplanta from uo_seleccion_plantas within w_aprobacion_cajas
integer x = 1902
integer y = 104
integer height = 92
integer taborder = 20
boolean bringtotop = true
boolean enabled = false
end type

on iuo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_3 from statictext within w_aprobacion_cajas
integer x = 183
integer y = 244
integer width = 393
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type st_4 from statictext within w_aprobacion_cajas
integer x = 1289
integer y = 244
integer width = 274
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Proceso"
boolean focusrectangle = false
end type

type sle_proceso from singlelineedit within w_aprobacion_cajas
integer x = 1595
integer y = 232
integer width = 402
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
end type

event modified;IF NOT BuscaOrden(Long(This.Text), ii_tiponum) THEN
	This.Text			=	''
	sle_mensa.text    =  ''
	This.SetFocus()
END IF
end event

type ddlb_tipoproc from dropdownlistbox within w_aprobacion_cajas
integer x = 645
integer y = 232
integer width = 603
integer height = 300
integer taborder = 30
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 1090519039
string text = "none"
string item[] = {"1.- Proceso","2.- Re Proceso","3.- Re Embalaje","4.- Pre Proceso"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;CHOOSE CASE Index
	CASE 2
		ii_tiponum	=	5
		
	CASE 3
		ii_tiponum	=	7
		
	CASE 4
		ii_tiponum	=	8
		
	CASE ELSE
		ii_tiponum	=	4
		
END CHOOSE
end event

type st_5 from statictext within w_aprobacion_cajas
integer x = 2053
integer y = 244
integer width = 210
integer height = 72
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Estado"
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_aprobacion_cajas
integer x = 2267
integer y = 232
integer width = 530
integer height = 92
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_aprobacion_cajas
boolean visible = false
integer x = 891
integer y = 704
integer width = 1202
integer height = 748
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_gtin14_nvofto_wm"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_3 from datawindow within w_aprobacion_cajas
boolean visible = false
integer y = 372
integer width = 155
integer height = 168
integer taborder = 90
boolean bringtotop = true
string title = "none"
string dataobject = "dw_mues_cajas_aprobacion"
boolean border = false
boolean livescroll = true
end type

