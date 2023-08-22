$PBExportHeader$w_pack_compactos_touch.srw
$PBExportComments$Ventana de emisión de compacto con pantalla tactil
forward
global type w_pack_compactos_touch from window
end type
type shl_2 from statichyperlink within w_pack_compactos_touch
end type
type shl_1 from statichyperlink within w_pack_compactos_touch
end type
type dw_planta from datawindow within w_pack_compactos_touch
end type
type dw_cliente from datawindow within w_pack_compactos_touch
end type
type pb_salir from picturebutton within w_pack_compactos_touch
end type
type dw_detalle from datawindow within w_pack_compactos_touch
end type
type dw_mercados from datawindow within w_pack_compactos_touch
end type
type dw_emb_calibres from datawindow within w_pack_compactos_touch
end type
type dw_crea_caja from datawindow within w_pack_compactos_touch
end type
type dw_7 from datawindow within w_pack_compactos_touch
end type
end forward

global type w_pack_compactos_touch from window
integer width = 4814
integer height = 2700
boolean titlebar = true
string title = "Emisión de Compacto"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
windowstate windowstate = maximized!
long backcolor = 21251140
string icon = "AppIcon!"
boolean center = true
event ue_recupera_informacion ( )
shl_2 shl_2
shl_1 shl_1
dw_planta dw_planta
dw_cliente dw_cliente
pb_salir pb_salir
dw_detalle dw_detalle
dw_mercados dw_mercados
dw_emb_calibres dw_emb_calibres
dw_crea_caja dw_crea_caja
dw_7 dw_7
end type
global w_pack_compactos_touch w_pack_compactos_touch

type variables
string is_mercado_ant, is_calibre_ant, is_embalaje_ant, is_calibre, is_embalaje, is_impresora, is_Computador
Str_mant			istr_mant
integer 			is_mercado
uo_cliente					iuo_clie
uo_manejoimpresora		iuo_impresora
Boolean						ib_impresora, ib_ImpresoraPrimerCambio
end variables

event ue_recupera_informacion();/*LRBB 26-Dic-2013*/
string ls_embalaje, ls_calibre
long ll_row, ll_fila, ll_col

dw_detalle.settransobject(sqlca)
dw_detalle.reset()
dw_emb_calibres.reset()
ll_fila = dw_detalle.retrieve(is_Computador)

if ll_fila > 0 then
	ls_embalaje = dw_detalle.getitemstring(1, 4)
	ll_row = dw_emb_calibres.insertrow(0)
	dw_emb_calibres.setitem(1, 1, ls_embalaje)
	ll_col = 0
	for ll_fila = 1 to dw_detalle.rowcount()
		if ls_embalaje = dw_detalle.getitemstring(ll_fila, 4) then
				ls_calibre = dw_detalle.getitemstring(ll_fila, 5)
			ll_col++
			dw_emb_calibres.setitem(ll_row, 1, ls_embalaje)
			dw_emb_calibres.setitem(ll_row, ll_col + 1, ls_calibre)
		else	
			 ll_row = dw_emb_calibres.insertrow(0)
			ll_col = 0
			ls_embalaje = dw_detalle.getitemstring(ll_fila, 4)
				  ls_calibre = dw_detalle.getitemstring(ll_fila, 5)
			dw_emb_calibres.setitem(ll_row, 1, ls_embalaje)
			dw_emb_calibres.setitem(ll_row, 2, ls_calibre)
			ll_col = 1
		 end if
	next
else
//	messagebox("Atención", "No existe orden de proceso activa...", stopsign!)
//	close(w_pack_las_delicias)
	return
end if

dw_emb_calibres.modify(is_calibre_ant+".background.mode=0")
dw_emb_calibres.modify(is_calibre_ant+".background.color=8421504")
dw_emb_calibres.modify("embalaje.background.mode=0")
dw_emb_calibres.modify("embalaje.background.color=8421504")
end event

on w_pack_compactos_touch.create
this.shl_2=create shl_2
this.shl_1=create shl_1
this.dw_planta=create dw_planta
this.dw_cliente=create dw_cliente
this.pb_salir=create pb_salir
this.dw_detalle=create dw_detalle
this.dw_mercados=create dw_mercados
this.dw_emb_calibres=create dw_emb_calibres
this.dw_crea_caja=create dw_crea_caja
this.dw_7=create dw_7
this.Control[]={this.shl_2,&
this.shl_1,&
this.dw_planta,&
this.dw_cliente,&
this.pb_salir,&
this.dw_detalle,&
this.dw_mercados,&
this.dw_emb_calibres,&
this.dw_crea_caja,&
this.dw_7}
end on

on w_pack_compactos_touch.destroy
destroy(this.shl_2)
destroy(this.shl_1)
destroy(this.dw_planta)
destroy(this.dw_cliente)
destroy(this.pb_salir)
destroy(this.dw_detalle)
destroy(this.dw_mercados)
destroy(this.dw_emb_calibres)
destroy(this.dw_crea_caja)
destroy(this.dw_7)
end on

event open;/*LRBB 26-Dic-2013*/
string ls_embalaje, ls_calibre
long ll_row, ll_fila, ll_col
datawindowchild idwc_cliente, idwc_planta

RegistryGet("HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\ComputerName\ComputerName", "ComputerName", RegString!, is_Computador)

this.backcolor = rgb(66, 68, 68)
shl_1.backcolor = rgb(66, 68, 68)
shl_2.backcolor = rgb(66, 68, 68)

iuo_clie			=	Create uo_cliente
iuo_impresora	=	Create uo_manejoimpresora

dw_mercados.settransobject(sqlca)
dw_mercados.retrieve()

dw_mercados.object.merc_nombre_1.background.mode="0" 
dw_mercados.object.merc_nombre_1.background.color="65280"
is_mercado_ant = "merc_nombre_1"
is_mercado = dw_mercados.object.merc_codigo[1]


dw_cliente.GetChild("clie_codigo", idwc_cliente)
idwc_cliente.SetTransObject(SQLCA)
idwc_cliente.Retrieve()
dw_cliente.SetTransObject(SQLCA)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_planta.GetChild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)
IF idwc_planta.Retrieve(gi_codexport)=0 THEN
	idwc_planta.insertrow(0)
END IF
dw_planta.SetTransObject(SQLCA)
dw_planta.InsertRow(0)
dw_planta.Object.plde_codigo[1]	=	gi_CodPlanta
dw_planta.Object.plde_codigo.width = 1000

ib_ImpresoraPrimerCambio	=	False

Timer(10)

This.TriggerEvent("ue_recupera_informacion")
end event

event timer;This.TriggerEvent("ue_recupera_informacion")
end event

type shl_2 from statichyperlink within w_pack_compactos_touch
integer x = 2318
integer y = 32
integer width = 274
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string pointer = "HyperLink!"
long textcolor = 12632256
long backcolor = 67108864
string text = "PLANTA"
boolean focusrectangle = false
end type

type shl_1 from statichyperlink within w_pack_compactos_touch
integer x = 581
integer y = 32
integer width = 288
integer height = 64
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string pointer = "HyperLink!"
long textcolor = 12632256
long backcolor = 67108864
string text = "CLIENTE"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_pack_compactos_touch
integer x = 2597
integer y = 20
integer width = 1138
integer height = 112
integer taborder = 20
boolean enabled = false
string title = "none"
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

type dw_cliente from datawindow within w_pack_compactos_touch
integer x = 873
integer y = 20
integer width = 1152
integer height = 112
integer taborder = 10
boolean enabled = false
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

type pb_salir from picturebutton within w_pack_compactos_touch
event mousemove pbm_mousemove
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 4411
integer y = 360
integer width = 300
integer height = 245
integer taborder = 40
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
string powertiptext = "Salir [Cerrar Ventana Activa]"
long backcolor = 553648127
end type

event clicked;Close(Parent)
end event

type dw_detalle from datawindow within w_pack_compactos_touch
boolean visible = false
integer x = 4443
integer y = 1428
integer width = 201
integer height = 152
integer taborder = 50
string title = "none"
string dataobject = "dw_detalle_ordenproc"
end type

type dw_mercados from datawindow within w_pack_compactos_touch
integer x = 37
integer y = 124
integer width = 4311
integer height = 200
integer taborder = 10
string title = "none"
string dataobject = "dw_mercados"
boolean border = false
boolean livescroll = true
end type

event clicked;/*LRBB 26-Dic-2013*/
string ls_columna

if row > 0 then
	dw_emb_calibres.modify(is_calibre_ant+".background.mode=0")
	dw_emb_calibres.modify(is_calibre_ant+".background.color=8388608")
	dw_emb_calibres.modify("embalaje.background.mode=0")
	dw_emb_calibres.modify("embalaje.background.color=8421504")

	ls_columna = dwo.name
	this.modify(is_mercado_ant+".background.mode=0")
	this.modify(is_mercado_ant+".background.color=8388608")

	this.modify(ls_columna+".background.mode=2")
	this.modify(ls_columna+".background.color=65280")
	is_mercado_ant = ls_columna
	is_mercado		 =	dw_mercados.getitemnumber(row, "merc_codigo")
end if

end event

type dw_emb_calibres from datawindow within w_pack_compactos_touch
event ue_genera_etiqueta ( )
integer x = 37
integer y = 324
integer width = 4311
integer height = 2152
integer taborder = 20
string title = "none"
string dataobject = "dw_emb_calibres"
boolean vscrollbar = true
boolean border = false
boolean livescroll = true
end type

event ue_genera_etiqueta();/*LRBB 26-Dic-2013*/
Integer 	li_filas, li_etiquetas, li_compactos, li_contratista, li_tipo, li_varrot, li_fila, li_categoria, li_etiqueta
Long		ll_proceso, ll_embala=-1
String	ls_embalaje, ls_calibre, ls_Registro, ls_calibrerot, ls_fecha, ls_codigo

		ll_proceso		=	dw_detalle.Object.orpr_numero[1]
		li_tipo				=	dw_detalle.Object.orpr_tipord[1]
		ls_calibrerot		=	is_calibre
		li_categoria		=	dw_detalle.Object.cate_codigo[1]
		li_etiqueta		=	dw_detalle.Object.etiq_codigo[1]	
  		li_varrot			=	dw_detalle.Object.vari_codigo[1]
		
		dw_crea_caja.SetTransObject(sqlca)
		dw_crea_caja.Reset()
	
		li_fila	=	dw_crea_caja.Retrieve(gi_CodExport, gi_CodPlanta,	&
														 	 li_tipo, 				&
														 	 ll_proceso, 			&
														 	 li_varrot, 			&
														 	 is_embalaje,		&
														 	 is_calibre, 			&
														 	 li_etiqueta, 			&
															 ll_embala, 			&
															 gstr_us.computador, & 
															 li_categoria, 		&
															 ls_calibrerot)


		IF li_fila = 1 THEN
				IF dw_crea_caja.Object.clie_codigo[1] <> -1 THEN
					is_impresora					=	dw_crea_caja.Object.impresora[1]
					dw_7.DataObject				=	dw_crea_caja.Object.formato[1]
				ELSE
					MessageBox("Error", "No se puede Generar Compacto.~r~n" + String(dw_crea_caja.Object.formato[1]))
					This.TriggerEvent("ue_imprimirerror")
					Return
				End If

        ELSE
				MessageBox("Error", "No se puede Generar Compacto.")
				This.TriggerEvent("ue_imprimirerror")
				Return
		End If
commit;


		iuo_clie.Existe(gi_CodExport, False, sqlca)

dw_7.SetTransObject(sqlca)

IF dw_7.Retrieve(gi_CodExport, gi_CodPlanta, 			&
					 dw_crea_caja.object.nrocaja[1],		& 
					 dw_crea_caja.object.nrocaja[1],		&
					is_mercado,									&
					'',												&
					0) > 0 then
						
	
		ls_fecha			=	String(dw_7.Object.capr_fecemb[1])
		ls_fecha			=	Left(ls_fecha, 2) + Mid(ls_fecha, 4, 2) + Right(ls_fecha, 2)
		ls_codigo			=	"01" + dw_7.Object.emba_nroint[1] + "10" + ls_fecha + "\F"
		ls_codigo			=	ls_codigo	+	"21" + String(dw_7.Object.plde_codigo[1], "0000") 
		ls_codigo			=	ls_codigo	+	String(dw_7.Object.capr_numero[1], '00000000')	
		
		dw_7.Object.Ole_1.Object.BarCode	=	iuo_clie.Barras
		dw_7.Object.Ole_1.Object.Text 		= 	ls_codigo
	
		  
	/*	IF IsNull(is_impresora) OR Len(String(is_impresora)) < 1 THEN
			ib_impresora	=	False
			
		ELSE
			iuo_impresora.asignaimpresora_comp(is_impresora)
			ib_impresora	=	True
			
		END IF
	
		IF iuo_impresora.is_impresoracomp <> '' AND ib_impresora THEN
			iuo_impresora.setimprcomp()
			dw_7.AcceptText()
			dw_7.Print(False, False)
			
			iuo_impresora.setimprdef()
			
		ELSE*/
			dw_7.AcceptText()
			dw_7.Print(False, False)
			
//		END IF
		
END IF
end event

event clicked;string ls_columna

if row > 0 then
	this.modify(is_calibre_ant+".background.mode=0")
	this.modify(is_calibre_ant+".background.color=8421504")
	this.modify("embalaje.background.mode=0")
	this.modify("embalaje.background.color=8421504")

	ls_columna = dwo.name
	if string(dwo.Type) = "column" and ls_columna <> "embalaje" then
	
		this.modify(ls_columna+".background.mode=2")
		this.modify(ls_columna+".background.color= '0~tIF (CurrentRow() = GetRow(), 65280, 8421504)'")
		
		this.modify("embalaje.background.mode=2")
		this.modify("embalaje.background.color= '0~tIF (CurrentRow() = GetRow(), 65280, 8421504)'")
	
		is_calibre_ant = ls_columna

		is_calibre		=	dw_emb_calibres.getitemstring(row, ls_columna)
		is_embalaje		=	dw_emb_calibres.Object.embalaje[Row]
		postevent ("ue_genera_etiqueta")
	end if
end if

end event

type dw_crea_caja from datawindow within w_pack_compactos_touch
boolean visible = false
integer x = 1989
integer y = 2316
integer width = 1477
integer height = 244
integer taborder = 60
string title = "none"
string dataobject = "dw_mant_creacion_cajas_granel"
boolean hscrollbar = true
boolean vscrollbar = true
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type dw_7 from datawindow within w_pack_compactos_touch
boolean visible = false
integer x = 146
integer y = 1444
integer width = 1445
integer height = 1116
integer taborder = 70
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_spro_cajasprod_gtin14"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

