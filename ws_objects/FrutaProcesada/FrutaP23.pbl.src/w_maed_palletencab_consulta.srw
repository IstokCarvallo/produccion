$PBExportHeader$w_maed_palletencab_consulta.srw
forward
global type w_maed_palletencab_consulta from w_maed_palletencab
end type
end forward

global type w_maed_palletencab_consulta from w_maed_palletencab
integer x = 14
integer y = 336
integer width = 3762
integer height = 2040
boolean titlebar = true
string title = "Consulta de Pallet"
boolean controlmenu = true
boolean border = true
windowtype windowtype = response!
end type
global w_maed_palletencab_consulta w_maed_palletencab_consulta

type variables
Str_mant	istr_mant2

DataWindowChild	dw_especies, dw_etiquetas, dw_plantas,&
						dw_condicioness,dw_embas,dw_embal
										  
Long			ll_tra,ll_def
Boolean		lb_Repalletizado = False
end variables

on w_maed_palletencab_consulta.create
call super::create
end on

on w_maed_palletencab_consulta.destroy
call super::destroy
end on

event ue_antesguardar;//
end event

event open;call super::open;/*
	Argumentos de istr_mant => Envío a Mantención de Detalle
		Argumento	[1]	=	Código de Exportador
						[2]	=	Número de Pallet
						[3]	=	Código de Especie
						[4]	=	Código de Variedad
						[5]	=	Nombre de Variedad
						[6]	=	Código de Planta
						[7]	=	Código de Embalaje
						[8]	=	Nombre del Embalaje
						[9]	=	Código de Etiqueta
						[10]	=	Código de Condición
						[11]	=	
						[12]	=	
						[13]	=	Nombre de Condición
						[14]	=	Nombre de Planta
						[15]	=	
						[16]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[20]	=	Tipo de Recepción : 1 Ingreso desde Packing => Recfruprocee_particular	
						[21]	=	Packing Origen


	Argumentos de istr_mant2 => Recepción por Ingreso o Consulta
		Argumento	[1]	=	Código de Planta
						[2]	=	Número de Folio Recepción
						[3]	=	Código de Exportador
						[4]	=	Cantidad de Tarjas
						[5]	=	Tipo de packing
						[6]	=	Número de Pallet
						[7]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[8]	=	
						[9]	=	Código de Especie
						[10]	=	Código de Variedad
						[11]	=	
						[12]	=	
						[13]	=	Nombre de Condición
						[14]	=	Nombre de Planta
						[15]	=	
						[16]	=	Condición de Sólo Consulta (0 = No / 1 = Si)
						[20]	=	Tipo de Recepción : 1 Ingreso desde Packing => Recfruprocee_particular	
						[21]	=	Packing Origen
*/
X	=	10
Y	=	300
This.Height	= 2020
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

/* La asignación a las siguientes variables no se hereda, debe ser adicional en ventana descendiente */
istr_mant2						=	Message.PowerObjectParm

dw_2.GetChild("clie_codigo", dw_cliente)
dw_2.GetChild("plde_codigo", dw_planta)
dw_2.GetChild("espe_codigo", dw_especie)
dw_2.GetChild("etiq_codigo", dw_etiqueta)
dw_2.GetChild("tpem_codigo", dw_embal)

dw_cliente.SetTransObject(sqlca)
dw_planta.SetTransObject(sqlca)
dw_especie.SetTransObject(sqlca)
dw_etiqueta.SetTransObject(sqlca)
dw_embal.SetTransObject(sqlca)

dw_cliente.Retrieve(Integer(istr_mant2.argumento[3]))
dw_planta.Retrieve(-1)
dw_especie.Retrieve(-1)
dw_etiqueta.Retrieve(-1)
dw_embal.Retrieve(Integer(istr_mant2.argumento[3]),'Z')

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

istr_mant.dw				=	dw_1
istr_mant.solo_consulta	=	True
dw_2.Enabled	=	False

buscar	= "Código:Nvari_codigo,Descripción:Svari_nombre"
ordenar	= "Código:vari_codigo,Descripción:vari_nombre"


dw_2.SetTabOrder("clie_codigo", 0)
dw_2.SetTabOrder("plde_codigo", 0)
dw_2.SetTabOrder("paen_numero", 0)
dw_2.Modify("clie_codigo.BackGround.Color = " + String(RGB(166,180,210)))
dw_2.Modify("plde_codigo.BackGround.Color = " + String(RGB(166,180,210)))
dw_2.Modify("paen_numero.BackGround.Color = " + String(RGB(166,180,210)))

istr_mant.Argumento[1]	=	istr_mant2.Argumento[3]
istr_mant.Argumento[2]	=	istr_mant2.Argumento[6]

This.TriggerEvent("ue_recuperadatos")

istr_mant.Argumento[3]	=	String(dw_2.Object.espe_codigo[1])
istr_mant.Argumento[4]	=	String(dw_2.Object.vari_codigo[1])
istr_mant.Argumento[5]	=	dw_2.Object.vari_nombre[1]
istr_mant.Argumento[6]	=	String(dw_2.Object.plde_codigo[1])
istr_mant.Argumento[7]	=	dw_2.Object.emba_codigo[1]
istr_mant.Argumento[8]	=	dw_2.Object.emba_nombre[1]
istr_mant.Argumento[9]	=	String(dw_2.Object.etiq_codigo[1])
istr_mant.Argumento[10] =	String(dw_2.Object.cond_codigo[1])
istr_mant.Argumento[12] =	""
istr_mant.Argumento[16]	=	istr_mant2.Argumento[7]	//	Estado sólo consulta
istr_mant.Argumento[20]	=	""
istr_mant.Argumento[21]	=	""

//PROBLEMA
ExisteTipoEmbalaje(String(dw_2.Object.tpem_codigo[1]))
ExistePlanta(dw_2.Object.plde_codigo[1])
ExisteCondicion(dw_2.Object.cond_codigo[1])
end event

event ue_guardar;//
end event

event close;GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
//
end event

event ue_nuevo;//
end event

event ue_seleccion;//
end event

event closequery;//
end event

event ue_modifica_detalle;
istr_mant.solo_consulta	=	True
call super::ue_modifica_detalle

end event

type dw_1 from w_maed_palletencab`dw_1 within w_maed_palletencab_consulta
integer x = 46
integer y = 1124
integer width = 3186
integer taborder = 110
end type

type dw_2 from w_maed_palletencab`dw_2 within w_maed_palletencab_consulta
integer x = 178
integer y = 28
integer width = 2930
integer height = 1032
end type

type pb_nuevo from w_maed_palletencab`pb_nuevo within w_maed_palletencab_consulta
boolean visible = false
end type

type pb_eliminar from w_maed_palletencab`pb_eliminar within w_maed_palletencab_consulta
boolean visible = false
end type

type pb_grabar from w_maed_palletencab`pb_grabar within w_maed_palletencab_consulta
boolean visible = false
integer x = 3291
integer y = 680
end type

type pb_imprimir from w_maed_palletencab`pb_imprimir within w_maed_palletencab_consulta
boolean visible = false
integer y = 900
end type

type pb_salir from w_maed_palletencab`pb_salir within w_maed_palletencab_consulta
end type

event pb_salir::clicked;Close(Parent)
end event

type pb_ins_det from w_maed_palletencab`pb_ins_det within w_maed_palletencab_consulta
boolean visible = false
end type

type pb_eli_det from w_maed_palletencab`pb_eli_det within w_maed_palletencab_consulta
boolean visible = false
end type

type pb_buscar from w_maed_palletencab`pb_buscar within w_maed_palletencab_consulta
boolean visible = false
end type

type dw_histoencab from w_maed_palletencab`dw_histoencab within w_maed_palletencab_consulta
end type

type dw_histofruta from w_maed_palletencab`dw_histofruta within w_maed_palletencab_consulta
end type

