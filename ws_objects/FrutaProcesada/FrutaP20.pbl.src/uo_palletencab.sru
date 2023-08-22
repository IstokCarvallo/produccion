$PBExportHeader$uo_palletencab.sru
forward
global type uo_palletencab from userobject
end type
type dw_1 from uo_dw within uo_palletencab
end type
end forward

global type uo_palletencab from userobject
integer width = 2907
integer height = 908
long backcolor = 12632256
string text = "none"
long tabtextcolor = 33554432
long picturemaskcolor = 536870912
dw_1 dw_1
end type
global uo_palletencab uo_palletencab

type variables
Str_mant	 			istr_mant2, istr_mant6

DataWindowChild	idwc_categorias, idwc_status, idwc_condicion, &
						idwc_tratamiento, idwc_tipofrio, idwc_destino, &
						dw_ptaori,dw_puerto, dw_especie, dw_etiqueta, dw_planta,&
						dw_cliente,dw_condiciones,dw_emba, dw_tpem
										  
Boolean				lb_MensPallet, lb_MensPucho,ib_existe_folio, ib_primera_entrada
Integer				ii_CantPallets, ii_CantPuchos

/*================================================================
Vector que guarda datos sobre la ubicacion de un pallet, el vector 
se compondrá por la sgte asignacion:
i_vec_ubicacion[1]=camara, i_vec_ubicacion[2]=calle, 
i_vec_ubicacion[3]=base,   i_vec_ubicacion[4]=posicion,
i_vec_ubicacion[5]=tipo camara
================================================================*/
integer 				i_vec_ubicacion[5], ii_yaexiste

protected:
Long					il_fila, il_AnchoDw_1
String					buscar, ordenar, ias_campo[]
Boolean				ib_datos_ok, ib_borrar, ib_ok, ib_traer, &
						ib_deshace = true, ib_ModEncab, ib_activado
Date					id_FechaAcceso
Time					it_HoraAcceso

Menu					im_menu

Str_parms			istr_parms
Str_mant				istr_mant
Str_busqueda		istr_busq
Str_info				istr_info
end variables

forward prototypes
public function string embalajecliente (integer ai_cliente)
public function boolean existevariedad (string ls_columna)
public function boolean existeembalaje (string ls_columna)
public function boolean existetipoembalaje (string as_codigo)
public function boolean existe_nrasda (string as_valor)
public function str_mant retornaestructura ()
public function integer retornacajas ()
end prototypes

public function string embalajecliente (integer ai_cliente);String	ls_Embalaje

SELECT	Min(emba_codigo)
	INTO	:ls_Embalaje
	FROM	dba.tipopallemba
	WHERE	clie_codigo	=	:ai_Cliente ;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura Tipos de Pallet por Embalaje")
END IF

RETURN ls_Embalaje
end function

public function boolean existevariedad (string ls_columna);Integer	li_cliente, li_especie, li_variedad
String	ls_nombre

li_cliente			=	Integer(istr_mant.argumento[1])
li_especie		=	dw_1.GetItemNumber(1, "espe_codigo")
li_variedad		=	Integer(ls_columna)

SELECT	vari_nombre
	INTO	:ls_nombre
	FROM	dba.variedades
	WHERE	espe_codigo	= 	:li_especie
	AND	vari_codigo = 	:li_variedad ;
				
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Variedades")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Variedad no Existe. Ingrese Otra.", Exclamation!, Ok!)
	RETURN False
ELSE
	istr_mant.argumento[3]	= String(li_especie)
	istr_mant.argumento[4]	= String(li_variedad)
	istr_mant.argumento[5]	= ls_nombre
	dw_1.SetItem(1, "vari_nombre", ls_nombre)
	//dw_seca.Retrieve(gi_codexport,li_especie,li_variedad)
	RETURN True
END IF

RETURN False
end function

public function boolean existeembalaje (string ls_columna);String	ls_codigo, ls_nombre
Integer	li_cliente, li_tipoen

li_cliente	= Integer(istr_mant.argumento[1])
ls_codigo	= ls_columna

SELECT	emb.emba_nombre, env.enva_tipoen
	INTO 	:ls_nombre, :li_tipoen
	FROM	dba.embalajesprod as emb, dba.envases as env
	WHERE	emb.clie_codigo	= :li_cliente
	AND	emb.emba_codigo	= :ls_codigo
	AND	env.enva_tipoen	= emb.enva_tipoen
	AND	env.enva_codigo	= emb.enva_codigo;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Emabalajes")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Código de Embalaje no Existe para este Cliente. Ingrese Otro.", Exclamation!, Ok!)
	RETURN False
ELSE
	istr_mant.argumento[7]	=	ls_codigo
	istr_mant.argumento[8]	=	ls_nombre
	dw_1.SetItem(1, "emba_nombre", ls_nombre)
	dw_1.SetItem(1, "tiem_codigo", li_tipoen)
	RETURN True
END IF

end function

public function boolean existetipoembalaje (string as_codigo);String	ls_embala
Integer	li_cliente, li_cancaj
Long		altura

li_cliente	= Integer(istr_mant.argumento[1])
ls_embala	= dw_1.Object.emba_codigo[1]

IF dw_1.Object.paen_tipopa[1]=2 THEN RETURN TRUE

SELECT	tpem_cancaj,tpem_altura
	INTO	:li_cancaj,:altura
	FROM	dba.tipopallemba
	WHERE	clie_codigo	=	:li_Cliente
	AND	emba_codigo	=	:ls_embala
	AND	tpem_codigo	=	:as_codigo ;
		
IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla TipoPallemba")
	RETURN False
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención","Tipo de Embalaje no Existe para este Cliente.~rIngrese Otro.", Exclamation!, Ok!)
	RETURN False
ELSE
	istr_mant.Argumento[11]	= String(li_cancaj)
	dw_1.SetItem(1,"paen_ccajas",li_cancaj)
	dw_1.SetItem(1,"paen_altura",altura)
//	Cuentacajas()
	RETURN True
END IF

RETURN False
end function

public function boolean existe_nrasda (string as_valor);Long	registros
  
  SELECT Count(*)  
    INTO :registros
    FROM dba.Palletencab
   WHERE paen_nrasda = :as_valor;

IF sqlca.SQLCode = -1 THEN
	F_errorbasedatos(sqlca,"Lectura tabla Palletfruta")
	RETURN TRUE
ELSEIF registros > 0 THEN
	MessageBox("Atención","Nro de ASDA ya fue ingresado en otro Registro", Exclamation!, Ok!)
	RETURN TRUE
END IF

RETURN FALSE
end function

public function str_mant retornaestructura ();istr_mant.argumento[1] = String(dw_1.GetItemNumber(1,"clie_codigo"))
istr_mant.argumento[2] = String(dw_1.GetItemNumber(1,"paen_numero"))
istr_mant.argumento[3] = String(dw_1.GetItemNumber(1,"espe_codigo"))
istr_mant.argumento[4] = String(dw_1.GetItemNumber(1,"vari_codigo"))
istr_mant.argumento[5] = dw_1.GetItemString(1,"vari_nombre")
istr_mant.argumento[6] = String(dw_1.GetItemNumber(1,"plde_codigo"))
istr_mant.argumento[7] = dw_1.GetItemString(1,"emba_codigo")
istr_mant.argumento[8] = dw_1.GetItemString(1,"emba_nombre")
istr_mant.argumento[9] = String(dw_1.GetItemNumber(1,"etiq_codigo"))
istr_mant.argumento[10] = String(dw_1.GetItemNumber(1,"cond_codigo"))
istr_mant.argumento[40] = String(dw_1.GetItemDate(1,"paen_fecemb"))
//str_mant.argumento[50] = String(dw_1.GetItemNumber(1,"prod_codigo"))
Return istr_mant
end function

public function integer retornacajas ();IF dw_1.RowCount() > 0 THEN
	Return dw_1.Object.paen_ccajas[1]
ELSE
	Return 0
END IF
end function

on uo_palletencab.create
this.dw_1=create dw_1
this.Control[]={this.dw_1}
end on

on uo_palletencab.destroy
destroy(this.dw_1)
end on

type dw_1 from uo_dw within uo_palletencab
integer width = 2885
integer height = 916
integer taborder = 10
string dataobject = "dw_mant_palletencab_reduc"
boolean vscrollbar = false
boolean border = false
boolean livescroll = true
end type

event itemchanged;call super::itemchanged;Long		ll_null, ll_PalletMan, ll_Data
String	ls_columna, ls_asda
Date		ld_nula
Integer	li_FilaMan
DataWIndowChild	dw_calibres

SetNull(ll_null)
SetNull(ld_nula)

ii_yaexiste	=	0
ls_columna = GetColumnName()

CHOOSE CASE ls_columna

	CASE "paen_inspec"
		IF Integer(data) = 1 THEN
			OpenWithParm(w_proc_inspeccion_informada, istr_mant3)
			istr_mant3 = Message.PowerObjectParm
		END IF

	CASE "paen_fecemb"
		istr_mant.Argumento[40] = Data

	CASE "clie_codigo"
		IF F_ValidaCliente(Integer(data)) THEN
			istr_mant.argumento[1]	= data
			dw_especie.Retrieve()
			dw_etiqueta.Retrieve()
			
			dw_1.GetChild("plde_codigo", dw_planta)
			dw_planta.SetTransObject(sqlca)
			dw_planta.Retrieve(1)
			dw_tpem.Retrieve(integer(data), istr_mant.argumento[37])	
			IF EmbalajeCliente(Integer(data)) = "" THEN
				MessageBox("Atención", "Cliente no tiene definido Tipos de Pallets" + &
								" por Embalaje.~r~rIngrese o seleccione otro Cliente.")
				dw_1.SetItem(1, "clie_codigo", gi_codexport)
				RETURN 1
			END IF	
		ELSE
			dw_1.SetItem(1, "clie_codigo", gi_codexport)
			RETURN 1
		END IF
		
	CASE "paen_numero"
		ll_PalletMan = dw_1.Object.paen_numero[1]
		li_FilaMan	 = Integer(GetRow())
		
		IF IsNull(ll_PalletMan) OR ll_PalletMan=0 THEN
			ll_Data = Long(Data)
		ELSE
			ll_Data = ll_PalletMan
		END IF
		
	CASE "espe_codigo"
		istr_mant.argumento[3]	= data
		dw_1.SetItem(1, "vari_nombre", "")
		dw_1.SetItem(1, "vari_codigo", ll_null)
				
	CASE "vari_codigo"
		IF ExisteVariedad(data) = False THEN
			dw_1.SetItem(1, "vari_nombre", "")
			dw_1.SetItem(1, "vari_codigo", ll_null)
			RETURN 1
		END IF
					
	CASE "plde_codigo"
		istr_mant.argumento[6]	= data
		
	CASE "emba_codigo"
		IF ExisteEmbalaje(data) = False THEN
			dw_1.SetItem(1, "emba_nombre", "")
			dw_1.SetItem(1, "emba_codigo", ll_null)
			RETURN 1
		ELSE
		 	dw_emba.Retrieve(Integer(istr_mant.argumento[1]),istr_mant.argumento[7])
			istr_mant.argumento[37] = data
			dw_tpem.Retrieve(integer(istr_mant.argumento[1]), data)	
		END IF
			
	CASE "tpem_codigo"
		IF dw_1.object.paen_tipopa[Row] = 1 THEN
			IF ExisteTipoEmbalaje(data) = False THEN
				dw_1.SetItem(1, "emba_codigo", ll_null)
				RETURN 1
			END IF
		END IF

	CASE "etiq_codigo"
		istr_mant.argumento[9]	= data
		
	CASE "cond_codigo"
		istr_mant.argumento[10]	= data
			
	CASE "paen_ccajas"
		istr_mant.argumento[11]	= data
			
	CASE "paen_pexpor"
		istr_mant.argumento[22]	= data
			
	CASE "paen_pmixto"
		istr_mant.argumento[23]	= data


	CASE "paen_fecemb"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, "paen_fecemb", ld_nula)
			RETURN 1
		ELSE
			istr_mant.Argumento[40] = Data
		END IF
		
	CASE "paen_cosecha"
		IF Not f_validafechatempo(date(data)) THEN
			This.SetItem(Row, "paen_cosecha", ld_nula)
			RETURN 1
		END IF
		
	CASE "paen_nrasda"
		IF data<>'' OR NOT IsNull(data)  THEN
			istr_mant.argumento[32] = 'RIOBL'+ data
			data = istr_mant.argumento[32]
		END IF
		
		IF Existe_nrasda(data) OR IsNull(data) THEN
			dw_1.SetItem(1, "paen_nrasda", String(ll_null))
			RETURN 1
		END IF
END CHOOSE

end event

event getfocus;call super::getfocus;integer 					li_cliente, li_planta
String  					ls_embalaje

IF Not(ib_activado) THEN
	istr_mant 				= 	message.PowerObjectParm
	
	dw_1.Object.clie_codigo.Protect	=	1
	dw_1.Object.plde_codigo.Protect	=	1
	dw_1.Object.clie_codigo.BackGround.Color = RGB(166,180,210)
	dw_1.Object.plde_codigo.BackGround.Color = RGB(166,180,210)
	
	li_cliente							=	Integer(istr_mant.Argumento[1])
	li_planta							=	Integer(istr_mant.Argumento[2])
	ls_Embalaje						=	EmbalajeCliente(li_Cliente)
	istr_mant.argumento[37] 	=  ""
	 
	 
	dw_1.GetChild("clie_codigo", dw_cliente)
	dw_1.GetChild("plde_codigo", dw_planta)
	dw_1.GetChild("espe_codigo", dw_especie)
	dw_1.GetChild("etiq_codigo", dw_etiqueta)
	dw_1.GetChild("tpem_codigo", dw_emba)
	dw_1.GetChild("tpem_codigo", dw_tpem)
	dw_1.GetChild("cate_codigo", idwc_categorias)
	dw_1.GetChild("stat_codigo", idwc_status)
	dw_1.GetChild("cond_codigo", idwc_condicion)
	dw_1.GetChild("trat_codigo", idwc_tratamiento)
	dw_1.GetChild("frio_codigo", idwc_tipofrio)
	dw_1.GetChild("dest_codigo", idwc_destino)
	
	dw_cliente.SetTransObject(sqlca)
	dw_planta.SetTransObject(sqlca)
	dw_especie.SetTransObject(sqlca)
	dw_etiqueta.SetTransObject(sqlca)
	dw_emba.SetTransObject(sqlca)
	dw_tpem.SetTransObject(sqlca)
	idwc_categorias.SetTransObject(sqlca)
	idwc_status.SetTransObject(sqlca)
	idwc_condicion.SetTransObject(sqlca)
	idwc_tratamiento.SetTransObject(sqlca)
	idwc_tipofrio.SetTransObject(sqlca)
	idwc_destino.SetTransObject(sqlca)
	
	dw_cliente.Retrieve(li_cliente)
	dw_planta.Retrieve(1)
	dw_especie.Retrieve()
	dw_etiqueta.Retrieve()
	dw_emba.Retrieve(li_cliente, ls_Embalaje)
	idwc_categorias.Retrieve()
	idwc_status.Retrieve()
	idwc_condicion.Retrieve()
	idwc_tratamiento.Retrieve()
	idwc_tipofrio.Retrieve()
	idwc_destino.Retrieve()
	dw_tpem.Retrieve(li_cliente, ls_Embalaje)
	dw_1.SetTransObject(sqlca)
	dw_1.Object.clie_codigo[1] 		= 	li_cliente
	dw_1.Object.plde_codigo[1] 	= 	li_planta
	dw_1.Object.paen_numero[1]	=	Long(istr_mant.Argumento[3])
	dw_1.SetColumn('paen_fecemb')
	
	ib_activado = TRUE
END IF
end event

