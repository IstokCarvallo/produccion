$PBExportHeader$w_info_solicitud_inspeccion_nuevo.srw
$PBExportComments$Emisión Solicitud de Inspección.
forward
global type w_info_solicitud_inspeccion_nuevo from w_para_informes
end type
type st_1 from statictext within w_info_solicitud_inspeccion_nuevo
end type
type dw_cliente from datawindow within w_info_solicitud_inspeccion_nuevo
end type
type st_2 from statictext within w_info_solicitud_inspeccion_nuevo
end type
type dw_plantadesp from datawindow within w_info_solicitud_inspeccion_nuevo
end type
type st_4 from statictext within w_info_solicitud_inspeccion_nuevo
end type
type ddlb_tipocond from dropdownlistbox within w_info_solicitud_inspeccion_nuevo
end type
type em_numero from editmask within w_info_solicitud_inspeccion_nuevo
end type
type st_3 from statictext within w_info_solicitud_inspeccion_nuevo
end type
type st_5 from statictext within w_info_solicitud_inspeccion_nuevo
end type
type cbx_terceros from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type cbx_varrot from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type st_7 from statictext within w_info_solicitud_inspeccion_nuevo
end type
type gb_3 from groupbox within w_info_solicitud_inspeccion_nuevo
end type
type st_8 from statictext within w_info_solicitud_inspeccion_nuevo
end type
type rb_aconcagua from radiobutton within w_info_solicitud_inspeccion_nuevo
end type
type rb_2 from radiobutton within w_info_solicitud_inspeccion_nuevo
end type
type rb_1 from radiobutton within w_info_solicitud_inspeccion_nuevo
end type
type cbx_prdrot from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type cbx_calrot from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type st_9 from statictext within w_info_solicitud_inspeccion_nuevo
end type
type cbx_archivo from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type cbx_packrot from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type rb_3 from radiobutton within w_info_solicitud_inspeccion_nuevo
end type
type cbx_1 from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type cbx_mexicopredios from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type cbx_id from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type cbx_formato from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type dw_1 from datawindow within w_info_solicitud_inspeccion_nuevo
end type
type dw_2 from datawindow within w_info_solicitud_inspeccion_nuevo
end type
type gb_4 from groupbox within w_info_solicitud_inspeccion_nuevo
end type
type st_6 from statictext within w_info_solicitud_inspeccion_nuevo
end type
type cbx_conex from checkbox within w_info_solicitud_inspeccion_nuevo
end type
type rb_detalle from radiobutton within w_info_solicitud_inspeccion_nuevo
end type
type rb_solicitud from radiobutton within w_info_solicitud_inspeccion_nuevo
end type
type ids_archivodw from datawindow within w_info_solicitud_inspeccion_nuevo
end type
type ids_archivo2dw from datawindow within w_info_solicitud_inspeccion_nuevo
end type
end forward

global type w_info_solicitud_inspeccion_nuevo from w_para_informes
integer width = 2670
integer height = 2160
string title = "SOLICITUD DE INSPECCION"
st_1 st_1
dw_cliente dw_cliente
st_2 st_2
dw_plantadesp dw_plantadesp
st_4 st_4
ddlb_tipocond ddlb_tipocond
em_numero em_numero
st_3 st_3
st_5 st_5
cbx_terceros cbx_terceros
cbx_varrot cbx_varrot
st_7 st_7
gb_3 gb_3
st_8 st_8
rb_aconcagua rb_aconcagua
rb_2 rb_2
rb_1 rb_1
cbx_prdrot cbx_prdrot
cbx_calrot cbx_calrot
st_9 st_9
cbx_archivo cbx_archivo
cbx_packrot cbx_packrot
rb_3 rb_3
cbx_1 cbx_1
cbx_mexicopredios cbx_mexicopredios
cbx_id cbx_id
cbx_formato cbx_formato
dw_1 dw_1
dw_2 dw_2
gb_4 gb_4
st_6 st_6
cbx_conex cbx_conex
rb_detalle rb_detalle
rb_solicitud rb_solicitud
ids_archivodw ids_archivodw
ids_archivo2dw ids_archivo2dw
end type
global w_info_solicitud_inspeccion_nuevo w_info_solicitud_inspeccion_nuevo

type variables
Str_mant				istr_mant
DataWindowChild	dwc_plantas
integer  ii_var

Transaction	Sqlprod

w_informes2				vinf2

uo_odbc				iuo_odbc
end variables

forward prototypes
public function boolean carga_terceros (integer ai_tipo, long al_numero, integer ai_cliente, integer ai_planta, integer ai_varrot, integer ai_prodrot, integer ai_calrot, integer ai_packrot, integer ai_marca, integer ai_info)
end prototypes

public function boolean carga_terceros (integer ai_tipo, long al_numero, integer ai_cliente, integer ai_planta, integer ai_varrot, integer ai_prodrot, integer ai_calrot, integer ai_packrot, integer ai_marca, integer ai_info);Long	ll_fila
Integer	li_conexion
String	ls_conexion,ls_nomodb,ls_nomser,ls_nombas,li_base,ls_Usuario,ls_Password,&
			ls_ubicacion,ls_ip,ls_puerto,ls_nodbms,ls_Motor
Boolean	lb_Conectado

Sqlprod	=	Create Transaction

FOR ll_fila =  1 TO dw_2.RowCount() 
		
	li_conexion	=	dw_2.Object.cone_codigo[ll_fila]
	ls_conexion	=	dw_2.Object.cone_descri[ll_fila]
	ls_nomodb	=	dw_2.Object.cone_nomodb[ll_fila]
	ls_nomser	=	dw_2.Object.cone_nomser[ll_fila]
	ls_nombas   =  dw_2.Object.cone_nombas[ll_fila]
	ls_nodbms	=	dw_2.Object.cone_nodbms[ll_fila]
	ls_Usuario	=	dw_2.Object.cone_nomusu[ll_fila]
	ls_Password	=	dw_2.Object.cone_passwo[ll_fila]	
	ls_ubicacion	=	dw_2.Object.cone_ubicac[ll_fila]
	ls_ip			=	dw_2.Object.cone_ipserv[ll_fila]
	ls_puerto	=	dw_2.Object.cone_puerto[ll_fila]	
			
	IF lb_Conectado THEN 
		DISCONNECT USING Sqlprod;
		
		lb_Conectado	=	False
	END IF
	
	datastore dw_motor

	dw_motor			=	create datastore
	iuo_odbc			=	Create uo_odbc
	
	dw_motor.dataObject='dw_mues_admamotorbd'
	
	dw_motor.SetTransObject(sqlca)
	
	ls_Motor = iuo_Odbc.MotorBD()
	
	IF ls_Motor = '' OR ls_ubicacion = '' OR ls_ip = '' OR ls_puerto = '' THEN
		MessageBox("Atención", "Falta mantención a tabla conexiones.")
		lb_Conectado = False
		Return False
	END IF	
	
	iuo_Odbc.Crea(ls_nomodb, ls_Usuario, ls_Password,&
		 ls_nombas,ls_ubicacion,ls_nomser,ls_ip, &
		 ls_puerto, ls_motor)

	Sqlprod.ServerName	=	ls_nomser
	Sqlprod.DataBase		=	ls_nombas
	Sqlprod.Dbms			= 	ls_nodbms
	Sqlprod.DbParm			=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
									";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'

	CONNECT USING Sqlprod;
NEXT

Return True
end function

on w_info_solicitud_inspeccion_nuevo.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_cliente=create dw_cliente
this.st_2=create st_2
this.dw_plantadesp=create dw_plantadesp
this.st_4=create st_4
this.ddlb_tipocond=create ddlb_tipocond
this.em_numero=create em_numero
this.st_3=create st_3
this.st_5=create st_5
this.cbx_terceros=create cbx_terceros
this.cbx_varrot=create cbx_varrot
this.st_7=create st_7
this.gb_3=create gb_3
this.st_8=create st_8
this.rb_aconcagua=create rb_aconcagua
this.rb_2=create rb_2
this.rb_1=create rb_1
this.cbx_prdrot=create cbx_prdrot
this.cbx_calrot=create cbx_calrot
this.st_9=create st_9
this.cbx_archivo=create cbx_archivo
this.cbx_packrot=create cbx_packrot
this.rb_3=create rb_3
this.cbx_1=create cbx_1
this.cbx_mexicopredios=create cbx_mexicopredios
this.cbx_id=create cbx_id
this.cbx_formato=create cbx_formato
this.dw_1=create dw_1
this.dw_2=create dw_2
this.gb_4=create gb_4
this.st_6=create st_6
this.cbx_conex=create cbx_conex
this.rb_detalle=create rb_detalle
this.rb_solicitud=create rb_solicitud
this.ids_archivodw=create ids_archivodw
this.ids_archivo2dw=create ids_archivo2dw
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_cliente
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_plantadesp
this.Control[iCurrent+5]=this.st_4
this.Control[iCurrent+6]=this.ddlb_tipocond
this.Control[iCurrent+7]=this.em_numero
this.Control[iCurrent+8]=this.st_3
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.cbx_terceros
this.Control[iCurrent+11]=this.cbx_varrot
this.Control[iCurrent+12]=this.st_7
this.Control[iCurrent+13]=this.gb_3
this.Control[iCurrent+14]=this.st_8
this.Control[iCurrent+15]=this.rb_aconcagua
this.Control[iCurrent+16]=this.rb_2
this.Control[iCurrent+17]=this.rb_1
this.Control[iCurrent+18]=this.cbx_prdrot
this.Control[iCurrent+19]=this.cbx_calrot
this.Control[iCurrent+20]=this.st_9
this.Control[iCurrent+21]=this.cbx_archivo
this.Control[iCurrent+22]=this.cbx_packrot
this.Control[iCurrent+23]=this.rb_3
this.Control[iCurrent+24]=this.cbx_1
this.Control[iCurrent+25]=this.cbx_mexicopredios
this.Control[iCurrent+26]=this.cbx_id
this.Control[iCurrent+27]=this.cbx_formato
this.Control[iCurrent+28]=this.dw_1
this.Control[iCurrent+29]=this.dw_2
this.Control[iCurrent+30]=this.gb_4
this.Control[iCurrent+31]=this.st_6
this.Control[iCurrent+32]=this.cbx_conex
this.Control[iCurrent+33]=this.rb_detalle
this.Control[iCurrent+34]=this.rb_solicitud
this.Control[iCurrent+35]=this.ids_archivodw
this.Control[iCurrent+36]=this.ids_archivo2dw
end on

on w_info_solicitud_inspeccion_nuevo.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_cliente)
destroy(this.st_2)
destroy(this.dw_plantadesp)
destroy(this.st_4)
destroy(this.ddlb_tipocond)
destroy(this.em_numero)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.cbx_terceros)
destroy(this.cbx_varrot)
destroy(this.st_7)
destroy(this.gb_3)
destroy(this.st_8)
destroy(this.rb_aconcagua)
destroy(this.rb_2)
destroy(this.rb_1)
destroy(this.cbx_prdrot)
destroy(this.cbx_calrot)
destroy(this.st_9)
destroy(this.cbx_archivo)
destroy(this.cbx_packrot)
destroy(this.rb_3)
destroy(this.cbx_1)
destroy(this.cbx_mexicopredios)
destroy(this.cbx_id)
destroy(this.cbx_formato)
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.gb_4)
destroy(this.st_6)
destroy(this.cbx_conex)
destroy(this.rb_detalle)
destroy(this.rb_solicitud)
destroy(this.ids_archivodw)
destroy(this.ids_archivo2dw)
end on

event open;call super::open;IF gi_vari_rotulada = 1 THEN
	cbx_varrot.Checked	= True
	cbx_varrot.Enabled	= False
ELSE
	cbx_varrot.Checked	= False
	cbx_varrot.Enabled	= True
END IF	

IF gi_prod_rotulado = 1 THEN
	cbx_prdrot.Checked	=	True
	cbx_prdrot.Enabled	=	False
ELSE
	cbx_prdrot.Checked	= 	False
	cbx_prdrot.Enabled	=	True
END IF

IF gi_cali_rotulado = 1 THEN
	cbx_calrot.Checked	=	True
	cbx_calrot.Enabled	=	False
ELSE
	cbx_calrot.Checked	= 	False
	cbx_calrot.Enabled	=	True
END IF

IF gi_pack_rotulado = 1 THEN
	cbx_packrot.Checked	=	True
	cbx_packrot.Enabled	=	False
ELSE
	cbx_packrot.Checked	= 	False
	cbx_packrot.Enabled	=	True
END IF

ii_var		= gi_vari_rotulada

dw_cliente.SetTransObject(Sqlca)
dw_cliente.InsertRow(0)
dw_cliente.SetItem(1, "clie_codigo", gi_CodExport)

dw_plantadesp.GetChild("plde_codigo", dwc_plantas)
dwc_plantas.SetTransObject(Sqlca)
dwc_plantas.Retrieve(1)			//Plantas de Despacho

dw_plantadesp.SetTransObject(Sqlca)
dw_plantadesp.InsertRow(0)
dw_plantadesp.SetItem(1,"plde_codigo", gi_CodPlanta)

ddlb_tipocond.SelectItem(1)

istr_mant.Argumento[1]	=	String(gi_CodExport)
istr_mant.Argumento[2]	=	String(gi_CodPlanta)
istr_mant.Argumento[3]	=	"1"

ids_archivo2				=	CREATE	DataStore
ids_archivo					=	CREATE	DataStore

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)

end event

type pb_excel from w_para_informes`pb_excel within w_info_solicitud_inspeccion_nuevo
end type

type st_computador from w_para_informes`st_computador within w_info_solicitud_inspeccion_nuevo
end type

type st_usuario from w_para_informes`st_usuario within w_info_solicitud_inspeccion_nuevo
end type

type st_temporada from w_para_informes`st_temporada within w_info_solicitud_inspeccion_nuevo
end type

type p_logo from w_para_informes`p_logo within w_info_solicitud_inspeccion_nuevo
integer width = 375
integer height = 304
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_info_solicitud_inspeccion_nuevo
integer width = 1989
string text = "Emisión Solicitud de Inspección"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_solicitud_inspeccion_nuevo
integer x = 2304
integer y = 1252
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		ll_Filas, li_prdrot, li_calrot, ll_numero, ll_Fila, ll_cont, ll_fil,ll_cantidad,ll_pallet, ll_tramoa, ll_tramob, ll_tramoc
Integer	li_varrot, li_Cliente, li_info, li_packrot, li_tipo, li_planta, li_marca, li_cont, li_conexion, li_cliente2
String	ls_Archivo, ls_Ruta, ls_Archivo2, ls_mensaje, ls_colu[],ls_conexion,ls_nomodb,ls_nomser,&
			ls_nombas,li_base,ls_Usuario,ls_Password,ls_ubicacion,ls_ip,ls_puerto,ls_nodbms,ls_Motor
Boolean	lb_Conectado
Dec		ld_peso, ld_kiloa, ld_kilob, ld_kiloc

str_info	lstr_info

dw_1.Reset()

ids_archivodw.SetTransObject(sqlca)
ids_archivo2dw.SetTransObject(sqlca)

If em_numero.Text = "" Then RETURN

lstr_info.titulo	= "SOLICITUD INSPECCION FITOSANITARIA S.A.G."
lstr_info.copias	= 1

OpenWithParm(vinf2,lstr_info)

If cbx_varrot.Checked Then
	li_varrot 	=	1
Else
	li_varrot	=	0
End If

If cbx_prdrot.Checked Then
	li_prdrot 	=	1
Else
	li_prdrot	=	0
End If

If rb_aconcagua.Checked Then
	li_calrot	=	-9
End If

If cbx_calrot.Checked Then
	li_calrot 	=	1
Else
	li_calrot	=	0
End If

If cbx_terceros.checked Then
	li_Cliente = -1
Else
	li_Cliente = dw_cliente.Object.clie_codigo[1]
End If

If cbx_packrot.Checked Then
	li_packrot 	=	1
Else
	li_packrot	=	0
End If

If cbx_id.Checked Then
	li_marca = 0
Else
	li_marca = 1
End If	

li_tipo		= Integer(istr_mant.argumento[3])
ll_numero	= Long(istr_mant.argumento[4])
li_planta 	= Integer(istr_mant.argumento[2])

If cbx_conex.Checked = False Then
	If cbx_formato.Checked = False Then
		If rb_aconcagua.Checked Then
			vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_aconcagua_nuevo"
			li_info	=	1
			ids_archivodw.DataObject	=	'dw_info_solicitud_inspeccion_acondet_pro'
			ids_archivo2dw.DataObject	=	'dw_info_solicitud_fitosan_acon_sag'
		ElseIf rb_2.Checked Then
			vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_compuesto_nuevo"
			
			li_info	=	2
			ids_archivodw.DataObject	=	'dw_info_solicitud_inspeccion_detalle_pro'
			ids_archivo2dw.DataObject	=	'dw_info_solicitud_fitosanitaria_sag'
			
		ElseIf rb_3.Checked Then
			If cbx_mexicopredios.checked Then	
				vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_sag_mexico_nuevo"
			Else
				vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_sag_mexico2_nuevo"
			End If
			li_info	=	4
			// este no esta
			ids_archivodw.DataObject	=	'dw_info_solicitud_inspeccion_unisag_pro_nuevo'
			ids_archivo2dw.DataObject	=	'dw_info_solicitud_fitosan_unisag_sag'
		Else	
			vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_unisag_nuevo"
			li_info	=	3
			ids_archivodw.DataObject	=	'dw_info_solicitud_inspeccion_unisag_pro'
			ids_archivo2dw.DataObject	=	'dw_info_solicitud_fitosan_unisag_sag'
		End If
	Else
		vinf2.dw_1.DataObject = "dw_info_sol_insp_sag_mexi_format_nuevo"
	End If	
	
	ls_Archivo	=	"\Inspeccion-"+String(Long(istr_mant.argumento[4]))+"Listado.xls"
	ls_Archivo2	=	"\Inspeccion-"+String(Long(istr_mant.argumento[4]))+"Fitosanitaria.xls"
	
	vinf2.dw_1.SetTransObject(sqlca)
		
	If cbx_formato.Checked <> False Then
		ll_Fila = dw_1.Retrieve(li_Cliente,Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]), &
											Long(istr_mant.argumento[4]),li_varrot,li_info,li_prdrot,li_calrot,li_packrot,li_marca)
		If ll_Fila > 0 Then
			FOR ll_cont = 1 TO dw_1.RowCount() 
				If Isnull(dw_1.Object.prpr_prepro[ll_cont]) OR dw_1.Object.prpr_prepro[ll_cont] = '' Then
					li_cont ++
					ls_mensaje 			= ls_mensaje + "~nFalta CSG para Predio del Productor "+ String(dw_1.Object.Prod_codigo[ll_cont])
					ls_colu[li_cont]	= "prpr_prepro"
				End If
				
				If Isnull(dw_1.Object.plde_codsag[ll_cont]) OR dw_1.Object.plde_codsag[ll_cont] = 0 Then
					li_cont ++
					ls_mensaje 			= ls_mensaje + "~nFalta Código Sag (CSP)para el Packing "+ String(dw_1.Object.pafr_copack[ll_cont])+' y Productor '+ String(dw_1.Object.Prod_codigo[ll_cont])
					ls_colu[li_cont]	= "plde_codsag"
				End If
			NEXT
			If li_cont > 0 Then MessageBox("Error de Consistencia", "Faltan Datos en Tablas :" + ls_mensaje + ".", StopSign!, Ok!)
		End If	
	End If
	
	ll_Filas = vinf2.dw_1.Retrieve(Integer(istr_mant.argumento[3]), &
					Long(istr_mant.argumento[4]), &
					li_Cliente, Integer(istr_mant.argumento[2]),li_varrot,&
					li_info,li_prdrot,li_calrot,li_packrot,li_marca)
End If	

If cbx_conex.Checked Then
	If cbx_formato.Checked = False Then
		If rb_aconcagua.Checked Then
			If rb_detalle.Checked Then
				vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_acondet_pro"
			Else	
				vinf2.dw_1.DataObject = "dw_info_solicitud_fitosan_acon_sag_nuevo"
			End If
			
			li_info	=	1
			ids_archivodw.DataObject	=	'dw_info_solicitud_inspeccion_acondet_pro'
		
		ElseIf rb_2.Checked Then
			If rb_detalle.Checked Then
				vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_detalle_pro"
			Else	
				vinf2.dw_1.DataObject = "dw_info_solicitud_fitosanitaria_sag_nuevo"
			End If
			li_info	=	2
			ids_archivodw.DataObject	=	'dw_info_solicitud_inspeccion_detalle_pro'
			ids_archivo2dw.DataObject	=	'dw_info_solicitud_fitosanitaria_sag'
		ElseIf rb_3.Checked Then
			If cbx_mexicopredios.checked Then	
				If rb_detalle.checked Then	
					vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_unisag_mexico"
				Else
					vinf2.dw_1.DataObject = "dw_info_solicitud_fitosan_unisag_sag_nuevo"
				End If				
			Else
				If rb_detalle.checked Then	
					vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_unisag_mexico2"
				Else
					vinf2.dw_1.DataObject = "dw_info_solicitud_fitosan_unisag_sag_nuevo"
				End If
			End If
			li_info	=	4
			ids_archivodw.DataObject	=	'dw_info_solicitud_inspeccion_unisag_pro_nuevo'
			ids_archivo2dw.DataObject	=	'dw_info_solicitud_fitosan_unisag_sag'
		Else	
			If rb_detalle.checked Then	
				vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_unisag_pro"
			Else
				vinf2.dw_1.DataObject = "dw_info_solicitud_fitosan_unisag_sag_nuevo"
			End If
						
			li_info	=	3
			ids_archivodw.DataObject	=	'dw_info_solicitud_inspeccion_unisag_pro'
			ids_archivo2dw.DataObject	=	'dw_info_solicitud_fitosan_unisag_sag'
		End If
	Else
		If rb_detalle.Checked Then
			vinf2.dw_1.DataObject = "dw_info_solicitud_inspeccion_unisag_mexico_nuevo"
		Else
			vinf2.dw_1.DataObject = "dw_info_sol_fitosan_sag_sag_format_nuevo_conec"
		End If	
	End If	
	
	ls_Archivo	=	"\Inspeccion-"+String(Long(istr_mant.argumento[4]))+"Listado.xls"
	ls_Archivo2	=	"\Inspeccion-"+String(Long(istr_mant.argumento[4]))+"Fitosanitaria.xls"
	
	vinf2.dw_1.SetTransObject(sqlca)
		
	If cbx_formato.Checked <> False Then
		
		ll_Fila = dw_1.Retrieve(li_Cliente,Integer(istr_mant.argumento[2]),Integer(istr_mant.argumento[3]), &
											Long(istr_mant.argumento[4]),li_varrot,li_info,li_prdrot,li_calrot,li_packrot,li_marca)
		
		If ll_Fila > 0 Then
			FOR ll_cont = 1 TO dw_1.RowCount() 
				If Isnull(dw_1.Object.prpr_prepro[ll_cont]) OR dw_1.Object.prpr_prepro[ll_cont] = '' Then
					li_cont ++
					ls_mensaje 			= ls_mensaje + "~nFalta CSG para Predio del Productor "+ String(dw_1.Object.Prod_codigo[ll_cont])
					ls_colu[li_cont]	= "prpr_prepro"
				End If
				
				If Isnull(dw_1.Object.plde_codsag[ll_cont]) OR dw_1.Object.plde_codsag[ll_cont] = 0 Then
					li_cont ++
					ls_mensaje 			= ls_mensaje + "~nFalta Código Sag (CSP)para el Packing "+ String(dw_1.Object.pafr_copack[ll_cont])+' y Productor '+ String(dw_1.Object.Prod_codigo[ll_cont])
					ls_colu[li_cont]	= "plde_codsag"
				End If
			NEXT
			If li_cont > 0 Then
					MessageBox("Error de Consistencia", "Faltan Datos en Tablas :" + ls_mensaje + ".", StopSign!, Ok!)
			End If
		End If	
	End If
	
	If cbx_archivo.Checked Then
		ids_archivodw.Retrieve(li_Cliente,Long(istr_mant.argumento[2]), &
								Integer(istr_mant.argumento[3]),long(istr_mant.argumento[4]),li_varrot,&
										li_info,li_prdrot,li_calrot,li_packrot,li_marca)
		
		ids_archivo2dw.Retrieve(li_Cliente,Long(istr_mant.argumento[2]), &
								Integer(istr_mant.argumento[3]),long(istr_mant.argumento[4]),li_varrot,li_prdrot,li_calrot)
	End If	
	
	If rb_detalle.Checked Then
		ll_Filas = vinf2.dw_1.Retrieve(li_Cliente,Integer(istr_mant.argumento[2]),&
				Integer(istr_mant.argumento[3]), Long(istr_mant.argumento[4]), &
						li_varrot,li_info,li_prdrot,li_calrot,li_packrot,li_marca)
	Else					
		ll_Filas = vinf2.dw_1.Retrieve(li_Cliente,Integer(istr_mant.argumento[2]),&
				Integer(istr_mant.argumento[3]), Long(istr_mant.argumento[4]), li_varrot,li_prdrot,li_calrot)
	End If						
	
	If cbx_terceros.checked Then
		dw_2.Reset()
		ll_fil = dw_2.Retrieve(1,-1) 
		If ll_fil > 0 Then
			Sqlprod	=	Create Transaction
			
			FOR ll_fila =  1 TO dw_2.RowCount() 
				li_conexion	=	dw_2.Object.cone_codigo[ll_fila]
				ls_conexion	=	dw_2.Object.cone_descri[ll_fila]
				ls_nomodb	=	dw_2.Object.cone_nomodb[ll_fila]
				ls_nomser	=	dw_2.Object.cone_nomser[ll_fila]
				ls_nombas   =  dw_2.Object.cone_nombas[ll_fila]
				ls_nodbms	=	dw_2.Object.cone_nodbms[ll_fila]
				ls_Usuario	=	dw_2.Object.cone_nomusu[ll_fila]
				ls_Password	=	dw_2.Object.cone_passwo[ll_fila]	
				ls_ubicacion	=	dw_2.Object.cone_ubicac[ll_fila]
				ls_ip			=	dw_2.Object.cone_ipserv[ll_fila]
				ls_puerto	=	dw_2.Object.cone_puerto[ll_fila]	
						
				If lb_Conectado Then 
					DISCONNECT USING Sqlprod;
					lb_Conectado	=	False
				End If
				
				iuo_odbc			=	Create uo_odbc
				ls_Motor = iuo_Odbc.MotorBD()
				
				If ls_Motor = '' OR ls_ubicacion = '' OR ls_ip = '' OR ls_puerto = '' Then
					MessageBox("Atención", "Falta mantención a tabla conexiones.")
					lb_Conectado = False
				End If	
				
				iuo_Odbc.Crea(ls_nomodb, ls_Usuario, ls_Password,ls_nombas,ls_ubicacion,ls_nomser,ls_ip, ls_puerto, ls_motor)
			
				Sqlprod.ServerName	=	ls_nomser
				Sqlprod.DataBase		=	ls_nombas
				Sqlprod.Dbms			= 	ls_nodbms
				Sqlprod.DbParm			=	"Connectstring='DSN=" + ls_nomodb + ";UID=" + ls_Usuario + &
												";PWD=" + ls_Password + "'// ;PBUseProcOwner = " + '"Yes"'
			
				CONNECT USING Sqlprod;
				
				If Sqlprod.SQLCode = 0 Then 
					lb_Conectado = True
															
					vinf2.dw_1.SetTransObject(Sqlprod)
					ids_archivodw.SetTransObject(Sqlprod)
					ids_archivo2dw.SetTransObject(Sqlprod)
					
					If rb_detalle.Checked Then
						ll_Filas = vinf2.dw_1.Retrieve(li_Cliente,Long(istr_mant.argumento[2]),&
								Integer(istr_mant.argumento[3]), Long(istr_mant.argumento[4]), &
								li_varrot,li_info,li_prdrot,li_calrot,li_packrot,li_marca)
								 vinf2.dw_1.GroupCalc()
					Else					
						ll_Filas = vinf2.dw_1.Retrieve(li_Cliente,Long(istr_mant.argumento[2]),&
								Integer(istr_mant.argumento[3]), Long(istr_mant.argumento[4]), &
								li_varrot,li_prdrot,li_calrot)
					End If	
					If cbx_archivo.Checked Then
						ids_archivodw.Retrieve(li_Cliente,Long(istr_mant.argumento[2]), &
												Integer(istr_mant.argumento[3]),long(istr_mant.argumento[4]),li_varrot,&
												li_info,li_prdrot,li_calrot,li_packrot,li_marca)
						
						ids_archivo2dw.Retrieve(li_Cliente,Long(istr_mant.argumento[2]), &
												Integer(istr_mant.argumento[3]),long(istr_mant.argumento[4]),li_varrot,li_prdrot,li_calrot)
					End If	
				End If	
			NEXT
		End If	
	End If
End If	

If cbx_conex.Checked Then
	If rb_solicitud.Checked Then
		ld_peso 		= 0
		ll_cantidad = 0
		ll_pallet 	= 0
		ld_kiloa		= 0
		ld_kilob		= 0
		ld_kiloc		= 0
		FOR ll_fila = 1 TO vinf2.dw_1.RowCount()
			li_cliente = vinf2.dw_1.Object.clie_codigo[ll_fila]
			If li_cliente2 <> li_cliente Then
				ll_cantidad = ll_cantidad + vinf2.dw_1.Object.compute_8[ll_fila]
				ld_peso 		= ld_peso + vinf2.dw_1.Object.compute_7[ll_fila]
				ll_pallet 	= ll_pallet + vinf2.dw_1.Object.compute_1[ll_fila]
				
				ld_kiloa = ld_kiloa + vinf2.dw_1.Object.kilosa[ll_fila]
				ld_kilob = ld_kilob + vinf2.dw_1.Object.kilosb[ll_fila]
				ld_kiloc = ld_kiloc + vinf2.dw_1.Object.kilosc[ll_fila]
				
				ll_tramoa = ll_tramoa + vinf2.dw_1.Object.tramoa[ll_fila]
				ll_tramob = ll_tramob + vinf2.dw_1.Object.tramob[ll_fila]
				ll_tramoc = ll_tramoc + vinf2.dw_1.Object.tramoc[ll_fila]
							
				li_cliente2 = li_cliente
			End If	
		NEXT	
	End If
End If	
	
If ll_Filas = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Filas = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
End If	

If cbx_archivo.Checked Then
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)	
	ids_archivodw.SaveAs(ls_Ruta + ls_Archivo,excel5!, True)
	ids_archivo2dw.SaveAs(ls_Ruta + ls_Archivo2,excel5!, True)
	MessageBox("Atención","Archivo Formato Excel, Generado.")
End If	

If gs_Ambiente = 'Windows' Then
	vinf2.dw_1.ModIfy('DataWindow.Print.Preview = Yes')
	vinf2.dw_1.ModIfy('DataWindow.Print.Preview.Zoom = 100')
	
	If ll_cantidad > 0 Then
		vinf2.dw_1.ModIfy("t_lotes.text = '" + String(ll_cantidad,'#,###') + "'")
		vinf2.dw_1.ModIfy("t_kilos.text = '" + String(ld_peso,'#,###.00') + "'")
		vinf2.dw_1.ModIfy("t_pallet.text = '" + String(ll_pallet,'#,###') + "'")
		
		vinf2.dw_1.ModIfy("t_kilosa.text = '" + String(ld_kiloa,'#,###.00') + "'")
		vinf2.dw_1.ModIfy("t_kilosb.text = '" + String(ld_kilob,'#,###.00') + "'")
		vinf2.dw_1.ModIfy("t_kilosc.text = '" + String(ld_kiloc,'#,###.00') + "'")
		
		vinf2.dw_1.ModIfy("t_tramoa.text = '" + String(ll_tramoa,'#,###') + "'")
		vinf2.dw_1.ModIfy("t_tramob.text = '" + String(ll_tramob,'#,###') + "'")
		vinf2.dw_1.ModIfy("t_tramoc.text = '" + String(ll_tramoc,'#,###') + "'")
	End If	
	
	If lb_Conectado Then 
		DISCONNECT USING Sqlprod;
		lb_Conectado	=	False
	End If
//	If vinf2.dw_1.Object.DataWindow.Print.Orientation = '1' Then
//		vinf2.dw_1.width = 5100//LandScape
//	ElseH
//		vinf2.dw_1.width = 3900//Portrait
//	End If
	If vinf2.dw_1.RowCount() > 0 Then
		vinf2.Visible	= 	True
		vinf2.Enabled	= 	True
	End If
End If
		
If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf2.dw_1, istr_info.titulo)

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_solicitud_inspeccion_nuevo
integer x = 2304
integer y = 1524
integer taborder = 60
end type

type st_1 from statictext within w_info_solicitud_inspeccion_nuevo
integer x = 329
integer y = 436
integer width = 347
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

type dw_cliente from datawindow within w_info_solicitud_inspeccion_nuevo
integer x = 677
integer y = 420
integer width = 1230
integer height = 96
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExisteCliente(Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[1] = data
	dwc_plantas.Retrieve(1)	
ELSE
	RETURN 1
END IF
end event

event itemerror;RETURN 1
end event

type st_2 from statictext within w_info_solicitud_inspeccion_nuevo
integer x = 329
integer y = 532
integer width = 347
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

type dw_plantadesp from datawindow within w_info_solicitud_inspeccion_nuevo
integer x = 677
integer y = 520
integer width = 969
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;String		ls_Columna[]

IF ExistePlanta(Integer(istr_mant.Argumento[1]), Integer(data), ls_Columna[]) THEN
	istr_mant.argumento[2] = data
ELSE
	RETURN 1
END IF
end event

type st_4 from statictext within w_info_solicitud_inspeccion_nuevo
integer x = 329
integer y = 636
integer width = 334
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
string text = "Condición"
boolean focusrectangle = false
end type

type ddlb_tipocond from dropdownlistbox within w_info_solicitud_inspeccion_nuevo
integer x = 677
integer y = 624
integer width = 526
integer height = 292
integer taborder = 30
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
boolean sorted = false
string item[] = {"Inspección","Re-Inspección"}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;istr_mant.argumento[3]	=	String(index)
end event

type em_numero from editmask within w_info_solicitud_inspeccion_nuevo
integer x = 677
integer y = 740
integer width = 402
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

event modified;istr_mant.argumento[4]	=	This.Text
end event

type st_3 from statictext within w_info_solicitud_inspeccion_nuevo
integer x = 329
integer y = 752
integer width = 251
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
string text = "Número"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_solicitud_inspeccion_nuevo
integer x = 242
integer y = 392
integer width = 1989
integer height = 472
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

type cbx_terceros from checkbox within w_info_solicitud_inspeccion_nuevo
integer x = 384
integer y = 872
integer width = 1253
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Incluye Terceros mismas Características"
end type

event clicked;IF This.Checked THEN
	dw_cliente.Enabled		=	False
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[1]	=	'-1'
	cbx_conex.Enabled = True
	cbx_conex.Checked = False
	
ELSE
	dw_cliente.Enabled		=	True
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	istr_mant.argumento[1]	=	String(dw_cliente.Object.clie_codigo[1])
	cbx_conex.Enabled = False
	cbx_conex.Checked = False
	rb_detalle.Enabled = False
	rb_detalle.Checked = False
	rb_solicitud.Enabled = False
	rb_solicitud.Checked = False
END IF
end event

type cbx_varrot from checkbox within w_info_solicitud_inspeccion_nuevo
integer x = 329
integer y = 1284
integer width = 649
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Variedad Rotulada"
boolean checked = true
end type

event clicked;IF This.Checked THEN
	dw_cliente.Enabled		=	False
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(166,180,210)
	istr_mant.argumento[1]	=	'-1'
ELSE
	dw_cliente.Enabled		=	True
	dw_cliente.Object.clie_codigo.BackGround.Color	=	RGB(255, 255, 255)
	istr_mant.argumento[1]	=	String(dw_cliente.Object.clie_codigo[1])
END IF
end event

type st_7 from statictext within w_info_solicitud_inspeccion_nuevo
integer x = 242
integer y = 1260
integer width = 1989
integer height = 216
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

type gb_3 from groupbox within w_info_solicitud_inspeccion_nuevo
integer x = 288
integer y = 1484
integer width = 1888
integer height = 380
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_8 from statictext within w_info_solicitud_inspeccion_nuevo
integer x = 242
integer y = 1476
integer width = 1989
integer height = 428
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

type rb_aconcagua from radiobutton within w_info_solicitud_inspeccion_nuevo
integer x = 567
integer y = 1696
integer width = 690
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
string text = "Formato Aconcagua"
end type

type rb_2 from radiobutton within w_info_solicitud_inspeccion_nuevo
integer x = 567
integer y = 1772
integer width = 635
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
string text = "Formato Genérico"
end type

type rb_1 from radiobutton within w_info_solicitud_inspeccion_nuevo
integer x = 567
integer y = 1544
integer width = 709
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
string text = "Formato Único S.A.G."
boolean checked = true
end type

type cbx_prdrot from checkbox within w_info_solicitud_inspeccion_nuevo
integer x = 1257
integer y = 1284
integer width = 850
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor Rotulado"
boolean checked = true
end type

type cbx_calrot from checkbox within w_info_solicitud_inspeccion_nuevo
integer x = 1257
integer y = 1368
integer width = 850
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Calibre Rotulado Repa  "
end type

type st_9 from statictext within w_info_solicitud_inspeccion_nuevo
integer x = 242
integer y = 1904
integer width = 1989
integer height = 100
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

type cbx_archivo from checkbox within w_info_solicitud_inspeccion_nuevo
integer x = 622
integer y = 1916
integer width = 320
integer height = 76
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
string text = "Archivo"
end type

type cbx_packrot from checkbox within w_info_solicitud_inspeccion_nuevo
integer x = 334
integer y = 1368
integer width = 695
integer height = 84
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Packing Rotulado"
end type

type rb_3 from radiobutton within w_info_solicitud_inspeccion_nuevo
integer x = 567
integer y = 1620
integer width = 709
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
string text = "Formato Mexico"
end type

type cbx_1 from checkbox within w_info_solicitud_inspeccion_nuevo
boolean visible = false
integer x = 2656
integer y = 988
integer width = 850
integer height = 116
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Productor Rotulado"
boolean checked = true
end type

type cbx_mexicopredios from checkbox within w_info_solicitud_inspeccion_nuevo
integer x = 1257
integer y = 1620
integer width = 850
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
string text = "Incluye Predios"
end type

type cbx_id from checkbox within w_info_solicitud_inspeccion_nuevo
integer x = 1792
integer y = 744
integer width = 343
integer height = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "ID"
end type

type cbx_formato from checkbox within w_info_solicitud_inspeccion_nuevo
integer x = 1298
integer y = 1916
integer width = 599
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
string text = "Formato Nuevo"
end type

event clicked;IF cbx_formato.Checked THEN
	rb_aconcagua.Enabled = False
	rb_2.Enabled = False
	rb_1.Enabled = False
	rb_3.Enabled = False
	cbx_mexicopredios.Enabled = False
ELSE
	rb_aconcagua.Enabled = True
	rb_2.Enabled = True
	rb_1.Enabled = True
	rb_3.Enabled = True
	cbx_mexicopredios.Enabled = True
END IF	

end event

type dw_1 from datawindow within w_info_solicitud_inspeccion_nuevo
boolean visible = false
integer x = 1170
integer width = 315
integer height = 212
integer taborder = 50
boolean bringtotop = true
string title = "none"
string dataobject = "dw_inspeccion_control_solicitud"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event retrievestart;Return 2
end event

type dw_2 from datawindow within w_info_solicitud_inspeccion_nuevo
boolean visible = false
integer x = 2350
integer y = 2116
integer width = 686
integer height = 400
integer taborder = 60
boolean bringtotop = true
string title = "none"
string dataobject = "dw_prodconectivad_inscondi"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type gb_4 from groupbox within w_info_solicitud_inspeccion_nuevo
integer x = 283
integer y = 924
integer width = 1911
integer height = 304
integer taborder = 50
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
end type

type st_6 from statictext within w_info_solicitud_inspeccion_nuevo
integer x = 242
integer y = 864
integer width = 1989
integer height = 392
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

type cbx_conex from checkbox within w_info_solicitud_inspeccion_nuevo
integer x = 439
integer y = 964
integer width = 1253
integer height = 100
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
string text = "Incluye Otras Conexiones"
end type

event clicked;IF This.Checked THEN
	rb_detalle.Enabled = True
	rb_detalle.Checked = True
	rb_solicitud.Enabled = True
	rb_solicitud.Checked = False
ELSE
	rb_detalle.Enabled = False
	rb_detalle.Checked = False
	rb_solicitud.Enabled = False
	rb_solicitud.Checked = False
END IF

IF This.Checked THEN
	rb_aconcagua.Enabled = False
	rb_2.Enabled = False
	rb_1.Enabled = False
	rb_3.Enabled = False
	cbx_mexicopredios.Enabled = False
	cbx_formato.Checked =True
	cbx_formato.Enabled =False
ELSE
	rb_aconcagua.Enabled = True
	rb_2.Enabled = True
	rb_1.Enabled = True
	rb_3.Enabled = True
	cbx_mexicopredios.Enabled = True
	cbx_formato.Enabled = True
	cbx_formato.Checked = False
END IF	

end event

type rb_detalle from radiobutton within w_info_solicitud_inspeccion_nuevo
integer x = 544
integer y = 1044
integer width = 1047
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
boolean enabled = false
string text = "Detalle del lote por Pallet"
end type

type rb_solicitud from radiobutton within w_info_solicitud_inspeccion_nuevo
integer x = 544
integer y = 1132
integer width = 1248
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
boolean enabled = false
string text = "Solicitud Inspeccion Fitosanitaria S.A.G."
end type

type ids_archivodw from datawindow within w_info_solicitud_inspeccion_nuevo
boolean visible = false
integer x = 2720
integer y = 2148
integer width = 686
integer height = 400
integer taborder = 30
boolean bringtotop = true
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type ids_archivo2dw from datawindow within w_info_solicitud_inspeccion_nuevo
boolean visible = false
integer x = 2784
integer y = 1632
integer width = 686
integer height = 400
integer taborder = 10
boolean bringtotop = true
string title = "none"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;Return 2
end event

